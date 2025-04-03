--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with GNAT.OS_Lib;

with Gnatformat.Command_Line;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Configuration;
with Gnatformat.Formatting;
with Gnatformat.Edits;
with Gnatformat.Project;

with GPR2;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;
with GPR2.Project.Tree;

with Langkit_Support.Slocs;
with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;

package body Gnatformat.Range_Format is

   use GNATCOLL.VFS;

   -------------------------------------
   --  Proceed_With_Range_Formatting  --
   -------------------------------------

   procedure Proceed_With_Range_Formatting
     (Project_File : GNATCOLL.VFS.Virtual_File)
   is
      use type Gnatformat.Command_Line.Sources.Result_Array;
      use type Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
      use Langkit_Support.Slocs;
      use Langkit_Support.Generic_API;

      Project_Tree : GPR2.Project.Tree.Object;

      CLI_Formatting_Config :
      constant Gnatformat.Configuration.Format_Options_Type :=
        Gnatformat.Command_Line.Configuration.Get;

      Diagnostics : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;

      Unparsing_Configuration_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Unparsing_Configuration.Get;
      Unparsing_Configuration      :
      constant Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Load_Unparsing_Configuration
          (Unparsing_Configuration_File, Diagnostics);

      Sources : constant Gnatformat.Command_Line.Sources.Result_Array :=
        Gnatformat.Command_Line.Sources.Get;

      Project_Format_Options_Cache :
      Gnatformat.Configuration.Project_Format_Options_Cache_Type :=
        Gnatformat.Configuration.Create_Project_Format_Options_Cache;

      Context : constant Libadalang.Analysis.Analysis_Context :=
        Libadalang.Analysis.Create_Context;

      Selection_Range : constant Source_Location_Range :=
        (Line_Number (Gnatformat.Command_Line.Start_Line.Get),
         Line_Number (Gnatformat.Command_Line.End_Line.Get),
         Column_Number (Gnatformat.Command_Line.Start_Column.Get),
         Column_Number (Gnatformat.Command_Line.End_Column.Get));

   begin
      --  Source line expected for range formatting
      if Sources = Gnatformat.Command_Line.Sources.No_Results
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "One source file must be provided.");
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            GNATCOLL.Opt_Parse.Help (Gnatformat.Command_Line.Parser));
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Only one source file is expected to be provided
      if Sources'Length > 1 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Only one source file is expected.");
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            GNATCOLL.Opt_Parse.Help (Gnatformat.Command_Line.Parser));
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  If project file is provides it is expected to be a valid .gpr
      --  and it will be used to get the project file specific frmatting
      --  settings if any
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

      --  The unparsing configuration is always initialized either with the
      --  one passed in the command line either with the default one
      if Unparsing_Configuration = Unparsing.No_Unparsing_Configuration then
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

      --  Load the project file
      if Project_File /= GNATCOLL.VFS.No_File then
         Gnatformat.Project.Load_Project (Project_Tree, Project_File);
      end if;

      if Project_Tree.Is_Defined then
         declare
            use Gnatformat.Project;
            Command_Line_Sources : constant Project_Source_Vector :=
              Get_Command_Line_Sources (Project_Tree);

            Format_Options : Gnatformat.Configuration.Format_Options_Type :=
              Gnatformat.Configuration.Get
                (Project_Format_Options_Cache,
                 Project_Tree.Root_Project);

            Source : constant Project_Source_Record :=
              Command_Line_Sources.First_Element;
            --  Get the source file since only one file is present in
            --  the given context

            Project_Formatting_Config :
            Gnatformat.Configuration.Format_Options_Type :=
              (case Source.Visible is
                  when True =>
                    Gnatformat.Configuration.Get
                 (Project_Format_Options_Cache,
                  Source.Visible_Source.Owning_View),
                  when False => Format_Options);

            Charset : constant String :=
              Ada.Strings.Unbounded.To_String
                (Gnatformat.Configuration.Get_Charset
                   (Project_Formatting_Config,
                    Source.File.Display_Base_Name));

            Unit    : constant Libadalang.Analysis.Analysis_Unit :=
              Context.Get_From_File
                (Source.File.Display_Full_Name (Normalize => True),
                 Charset);

            Edits   : Gnatformat.Edits.Formatting_Edit_Type;

         begin
            Gnatformat.Configuration.Overwrite
              (Format_Options, CLI_Formatting_Config);
            Edits :=
              Gnatformat.Formatting.Range_Format
                (Unit            => Unit,
                 Selection_Range => Selection_Range,
                 Format_Options  => Format_Options,
                 Configuration   => Unparsing_Configuration);

            if Gnatformat.Command_Line.Pipe.Get then
               Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
               Ada.Text_IO.New_Line;
            end if;
         end;
      else
         --  Process the only one source file present in the given context
         --  when no project tree is defined
         if not Sources (Sources'First).Is_Regular_File then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Failed to find "
               & Sources (Sources'First).Display_Base_Name);
            return;
         else
            declare
               use Gnatformat.Configuration;
               Source : constant GNATCOLL.VFS.Virtual_File :=
                 Sources (Sources'First);
               Charset :
               constant Optional_Unbounded_String :=
                 Gnatformat.Command_Line.Charset.Get;
               Unit    : constant Libadalang.Analysis.Analysis_Unit :=
                 Context.Get_From_File
                   (Source.Display_Full_Name (Normalize => True),
                    (if Charset.Is_Set then
                            Ada.Strings.Unbounded.To_String (Charset.Value)
                     else
                        Default_Charset));
               Edits : Gnatformat.Edits.Formatting_Edit_Type;
            begin
               Edits :=
                 Gnatformat.Formatting.Range_Format
                   (Unit            => Unit,
                    Selection_Range => Selection_Range,
                    Format_Options  =>
                      Gnatformat.Command_Line.Configuration.Get,
                    Configuration   => Unparsing_Configuration);

               if Gnatformat.Command_Line.Pipe.Get then
                  Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
                  Ada.Text_IO.New_Line;
               end if;
            end;
         end if;

      end if;
   end Proceed_With_Range_Formatting;

end Gnatformat.Range_Format;
