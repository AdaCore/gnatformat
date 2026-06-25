--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with GNAT.OS_Lib;

procedure Git_Format is
   --  Thin "git gnatformat" subcommand wrapper.
   --
   --  Git exposes any executable named "git-foo" on the PATH as the
   --  "git foo" subcommand. This program is installed as "git-gnatformat"
   --  next to the much larger "gnatformat" binary and merely forwards
   --
   --     git gnatformat [<base-commit>] [<extra args>]
   --
   --  to
   --
   --     gnatformat --gitdiff <base-commit> [<extra args>]
   --
   --  An omitted base commit defaults to HEAD. Keeping this a separate,
   --  dependency-free executable (it links against nothing but the Ada
   --  runtime) avoids shipping a ~57MB second copy of the gnatformat binary
   --  just to provide the subcommand.

   use type GNAT.OS_Lib.String_Access;

   Gnatformat_Executable_Name : constant String := "gnatformat";

   procedure Print_Usage;
   --  Print a short usage paragraph for the "git gnatformat" subcommand.

   function Help_Requested return Boolean;
   --  True when the first argument requests usage (-h / --help).

   function Locate_Gnatformat return GNAT.OS_Lib.String_Access;
   --  Locate the "gnatformat" executable, preferring the one sitting next to
   --  this wrapper (the expected install layout) and falling back to a PATH
   --  lookup. Returns null when it cannot be found.

   procedure Print_Usage is
   begin
      Ada.Text_IO.Put_Line
        ("usage: git gnatformat [<base-commit>] [<gnatformat options>]");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("Format the Ada lines added or changed since <base-commit> "
         & "(default: HEAD).");
      Ada.Text_IO.Put_Line
        ("This is a thin Git subcommand wrapper around "
         & "'gnatformat --gitdiff <base-commit>';");
      Ada.Text_IO.Put_Line
        ("any extra arguments are forwarded to gnatformat unchanged.");
   end Print_Usage;

   function Help_Requested return Boolean
   is (Ada.Command_Line.Argument_Count >= 1
       and then
         (Ada.Command_Line.Argument (1) = "-h"
          or else Ada.Command_Line.Argument (1) = "--help"));

   function Locate_Gnatformat return GNAT.OS_Lib.String_Access is
      Self : constant String := Ada.Command_Line.Command_Name;
   begin
      --  First look right next to this wrapper, so the subcommand resolves to
      --  the sibling install even when it is not first on the PATH.
      declare
         Self_Directory : constant String :=
           Ada.Directories.Containing_Directory (Self);
         Sibling        : constant String :=
           GNAT.OS_Lib.Normalize_Pathname
             (Gnatformat_Executable_Name, Self_Directory);
      begin
         if GNAT.OS_Lib.Is_Executable_File (Sibling) then
            return new String'(Sibling);
         end if;
      end;

      return GNAT.OS_Lib.Locate_Exec_On_Path (Gnatformat_Executable_Name);
   exception
      when others =>
         --  Containing_Directory raises when Self carries no directory part;
         --  fall back to the PATH lookup in that case.
         return GNAT.OS_Lib.Locate_Exec_On_Path (Gnatformat_Executable_Name);
   end Locate_Gnatformat;

begin
   if Help_Requested then
      Print_Usage;
      return;
   end if;

   declare
      Count : constant Natural := Ada.Command_Line.Argument_Count;

      Has_Base : constant Boolean :=
        Count >= 1
        and then Ada.Command_Line.Argument (1)'Length > 0
        and then Ada.Command_Line.Argument (1) (1) /= '-';
      --  A leading argument that does not look like an option is taken as the
      --  base commit; otherwise the base defaults to HEAD.

      Base        : constant String :=
        (if Has_Base then Ada.Command_Line.Argument (1) else "HEAD");
      First_Extra : constant Positive := (if Has_Base then 2 else 1);
      Extra_Count : constant Natural := Count - First_Extra + 1;

      Gnatformat : GNAT.OS_Lib.String_Access := Locate_Gnatformat;

      Arguments : GNAT.OS_Lib.Argument_List (1 .. 2 + Extra_Count);

      Exit_Status : Integer;
   begin
      if Gnatformat = null then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "git-gnatformat: could not find the 'gnatformat' executable; "
            & "make sure it is installed on your PATH.");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Arguments (1) := new String'("--gitdiff");
      Arguments (2) := new String'(Base);
      for I in 0 .. Extra_Count - 1 loop
         Arguments (3 + I) :=
           new String'(Ada.Command_Line.Argument (First_Extra + I));
      end loop;

      Exit_Status := GNAT.OS_Lib.Spawn (Gnatformat.all, Arguments);

      for Argument of Arguments loop
         GNAT.OS_Lib.Free (Argument);
      end loop;
      GNAT.OS_Lib.Free (Gnatformat);

      GNAT.OS_Lib.OS_Exit (Exit_Status);
   end;
end Git_Format;
