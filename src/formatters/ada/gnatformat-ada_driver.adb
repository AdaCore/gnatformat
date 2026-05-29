--
--  Copyright (C) 2024-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gnatformat.Abstract_Writers;
with Gnatformat.Bail;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Command_Line;
with Gnatformat.Configuration;
with Gnatformat.Console_Writers;
with Gnatformat.File_Writers;
with Gnatformat.Full_Format;
with Gnatformat.Project;
with Gnatformat.Range_Format;

with GPR2;
with GPR2.Options;
with GPR2.Project.Tree;

with Langkit_Support.Slocs;

procedure Gnatformat.Ada_Driver is

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
      if Gnatformat.Command_Line.Parser.Last_Error /= "" then
         Gnatformat.Bail.Bail (1);
      else
         Gnatformat.Bail.Bail (0);
      end if;
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
      No_Project   : constant Boolean :=
        Gnatformat.Project.GPR_Options.No_Project;
      Project_File : constant GNATCOLL.VFS.Virtual_File :=
        (if No_Project
         then GNATCOLL.VFS.No_File
         else Gnatformat.Project.Resolve_Project_File);
      Project_Tree : GPR2.Project.Tree.Object;

      CLI_Formatting_Config :
        constant Gnatformat.Configuration.Format_Options_Type :=
          Gnatformat.Command_Line.Configuration.Get;

      Unparsing_Configuration_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Unparsing_Configuration.Get;

      Sources : constant Gnatformat.Command_Line.Sources.Result_Array :=
        Gnatformat.Command_Line.Sources.Get;

      Writer : Gnatformat.Abstract_Writers.Abstract_Writer'Class :=
        (if Gnatformat.Command_Line.Pipe.Get
         then
           Gnatformat.Console_Writers.Writer
             (Single_File => Sources'Length = 1)
         else Gnatformat.File_Writers.Writer);

      use type Gnatformat.Command_Line.Sources.Result_Array;

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
            Gnatformat.Bail.Bail (1);
         end if;

      else
         --  Confirm that either a project and/or sources files were provided

         if (No_Project or Project_File = GNATCOLL.VFS.No_File)
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
            Gnatformat.Bail.Bail (1);
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
         Gnatformat.Bail.Bail (1);
      end if;

      if No_Project then
         Gnatformat.Gnatformat_Trace.Trace
           ("Proceeding with no project loaded");

      elsif Project_File /= GNATCOLL.VFS.No_File then
         Gnatformat.Project.Load_Project (Project_Tree, Project_File);
      end if;

      if Gnatformat.Command_Line.Range_Format.Get then
         Gnatformat.Range_Format.Range_Format
           (Project_Tree                 => Project_Tree,
            Source                       => Sources (Sources'First),
            Selection_Range              =>
              (Langkit_Support.Slocs.Line_Number
                 (Gnatformat.Command_Line.Start_Line.Get),
               Langkit_Support.Slocs.Line_Number
                 (Gnatformat.Command_Line.End_Line.Get),
               Langkit_Support.Slocs.Column_Number
                 (Gnatformat.Command_Line.Start_Column.Get),
               Langkit_Support.Slocs.Column_Number
                 (Gnatformat.Command_Line.End_Column.Get)),
            CLI_Formatting_Config        => CLI_Formatting_Config,
            Unparsing_Configuration_File => Unparsing_Configuration_File,
            Default_Charset              => Charset,
            Pipe                         => Gnatformat.Command_Line.Pipe.Get);

      else
         Gnatformat.Full_Format.Full_Format
           (Writer,
            Project_Tree,
            CLI_Formatting_Config,
            Unparsing_Configuration_File,
            Sources,
            Format_Options => Gnatformat.Command_Line.Configuration.Get,
            Check          => Gnatformat.Command_Line.Check.Get,
            Keep_Going     => Gnatformat.Command_Line.Keep_Going.Get,
            Charset        => Charset,
            Base_Commit_ID => Gnatformat.Command_Line.Gitdiff.Get);
      end if;
   end;

exception

   when Gnatformat.Bail.Bail_Out =>
      --  Deliberate non-local exit. Falling through here lets finalization
      --  run. See Gnatformat.Bail for the rationale.
      null;

   when E : others =>
      --  Catch unhandled exceptions at the top level so we exit cleanly.
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Unexpected error: "
         & Ada.Exceptions.Exception_Name (E)
         & ": "
         & Ada.Exceptions.Exception_Message (E));
      Gnatformat_Trace.Trace (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Gnatformat.Ada_Driver;
