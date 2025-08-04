--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gnatformat.Command_Line;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Configuration;
with Gnatformat.Full_Format;
with Gnatformat.Range_Format;
with Gnatformat.Project;

with GPR2;
with GPR2.Options;
with GPR2.Project.Tree;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Slocs;

procedure Gnatformat.Ada_Driver is

   package Langkit_Support_Unparsing renames
     Langkit_Support.Generic_API.Unparsing;

   function Charset return String
   is (declare
         CLI_Charset :
           constant Gnatformat.Configuration.Optional_Unbounded_String :=
             Gnatformat.Command_Line.Charset.Get;
       begin
         (if CLI_Charset.Is_Set
          then Ada.Strings.Unbounded.To_String (CLI_Charset.Value)
          else Gnatformat.Configuration.Default_Charset));
   --  Return charset from command line option or default one if none.

begin
   GNATCOLL.Traces.Parse_Config_File;

   if not Gnatformat.Command_Line.Parser.Parse then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Failed to parse CLI arguments");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if Gnatformat.Command_Line.Version.Get then
      Ada.Text_IO.Put_Line
        ("GNATformat "
         & Gnatformat.Version
         & " ("
         & Gnatformat.Build_Date
         & ")");
      return;
   end if;

   if Gnatformat.Command_Line.Verbose.Get then
      Gnatformat_Trace.Set_Active (True);
   end if;

   Gnatformat_Trace.Trace
     ("GNATformat " & Gnatformat.Version & " " & Gnatformat.Build_Date);

   Gnatformat.Configuration.Elaborate_GPR2;

   Gnatformat.Project.GPR_Options :=
     Gnatformat.Command_Line.GPR_Args.Parsed_GPR2_Options;

   Gnatformat.Project.GPR_Options.Print_GPR_Registry;

   declare
      Project_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Project.Resolve_Project_File;
      Project_Tree : GPR2.Project.Tree.Object;

      CLI_Formatting_Config :
        constant Gnatformat.Configuration.Format_Options_Type :=
          Gnatformat.Command_Line.Configuration.Get;

      Diagnostics : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;

      Unparsing_Configuration_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Unparsing_Configuration.Get;
      Unparsing_Configuration      :
        constant Langkit_Support_Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Load_Unparsing_Configuration
            (Unparsing_Configuration_File, Diagnostics);

      Sources : constant Gnatformat.Command_Line.Sources.Result_Array :=
        Gnatformat.Command_Line.Sources.Get;

      use type Gnatformat.Command_Line.Sources.Result_Array;
      use type Langkit_Support_Unparsing.Unparsing_Configuration;

   begin
      if Gnatformat.Command_Line.Range_Format.Get then
         --  Confirm that exactly one source file was provided

         if Sources = Gnatformat.Command_Line.Sources.No_Results
           or else Sources'Length /= 1
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Exactly one source file must be provided.");
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

      else
         --  Confirm that either a project and/or sources files were provided

         if Project_File = GNATCOLL.VFS.No_File
           and Sources = Gnatformat.Command_Line.Sources.No_Results
           and not Gnatformat.Command_Line.Gitdiff.Get.Is_Set
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Either a project or sources must be provided.");
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Gnatformat.Command_Line.Parser.Help);
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end if;

      if Project_File /= GNATCOLL.VFS.No_File
        and not GNATCOLL.VFS.Is_Regular_File (Project_File)
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Provided project file """
            & Project_File.Display_Full_Name
            & """ does not exit.");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if Unparsing_Configuration
        = Langkit_Support_Unparsing.No_Unparsing_Configuration
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Failed to load unparsing configuration");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if not Diagnostics.Is_Empty then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "Failed to load formatting rules");
         for Diagnostic of Diagnostics loop
            Gnatformat.Gnatformat_Trace.Trace
              (Langkit_Support.Diagnostics.To_Pretty_String (Diagnostic));
         end loop;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if Project_File /= GNATCOLL.VFS.No_File then
         Gnatformat.Project.Load_Project (Project_Tree, Project_File);
      end if;

      if Gnatformat.Command_Line.Range_Format.Get then
         Gnatformat.Range_Format.Range_Format
           (Project_Tree            => Project_Tree,
            Source                  => Sources (Sources'First),
            Selection_Range         =>
              (Langkit_Support.Slocs.Line_Number
                 (Gnatformat.Command_Line.Start_Line.Get),
               Langkit_Support.Slocs.Line_Number
                 (Gnatformat.Command_Line.End_Line.Get),
               Langkit_Support.Slocs.Column_Number
                 (Gnatformat.Command_Line.Start_Column.Get),
               Langkit_Support.Slocs.Column_Number
                 (Gnatformat.Command_Line.End_Column.Get)),
            CLI_Formatting_Config   => CLI_Formatting_Config,
            Unparsing_Configuration => Unparsing_Configuration,
            Default_Charset         => Charset,
            Pipe                    => Gnatformat.Command_Line.Pipe.Get);

      else
         Gnatformat.Full_Format.Full_Format
           (Project_Tree,
            CLI_Formatting_Config,
            Unparsing_Configuration,
            Sources,
            Charset => Charset);
      end if;
   end;
end Gnatformat.Ada_Driver;
