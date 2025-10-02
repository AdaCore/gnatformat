--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with Gnatformat.Formatting;
with Gnatformat.Edits;
with Gnatformat.Project;

with GPR2;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;

with Libadalang.Analysis;

package body Gnatformat.Range_Format is

   --------------------
   --  Range_Format  --
   --------------------

   procedure Range_Format
     (Project_Tree            : GPR2.Project.Tree.Object;
      Source                  : GNATCOLL.VFS.Virtual_File;
      Selection_Range         : Langkit_Support.Slocs.Source_Location_Range;
      CLI_Formatting_Config   : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Configuration :
        Langkit_Support_Unparsing.Unparsing_Configuration;
      Default_Charset         : String :=
        Gnatformat.Configuration.Default_Charset;
      Pipe                    : Boolean := False) is
   begin
      if Project_Tree.Is_Defined then
         Project_Tree.Update_Sources;

         declare
            Project_Source :
              constant Gnatformat.Project.Project_Source_Record :=
                Gnatformat.Project.To_Project_Source (Project_Tree, Source);

            Project_Format_Options_Cache :
              Gnatformat.Configuration.Project_Format_Options_Cache_Type :=
                Gnatformat.Configuration.Create_Project_Format_Options_Cache;

            Format_Options : Gnatformat.Configuration.Format_Options_Type :=
              Gnatformat.Configuration.Get
                (Project_Format_Options_Cache, Project_Tree.Root_Project);

            Project_Formatting_Config :
              Gnatformat.Configuration.Format_Options_Type :=
                (case Project_Source.Visible is
                   when True  =>
                     Gnatformat.Configuration.Get
                       (Project_Format_Options_Cache,
                        Project_Source.Visible_Source.Owning_View),
                   when False => Format_Options);

            Charset : constant String :=
              Ada.Strings.Unbounded.To_String
                (Gnatformat.Configuration.Get_Charset
                   (Project_Formatting_Config,
                    Project_Source.File.Display_Base_Name));

            Context : constant Libadalang.Analysis.Analysis_Context :=
              Libadalang.Analysis.Create_Context;

            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              Context.Get_From_File
                (Project_Source.File.Display_Full_Name (Normalize => True),
                 Charset);

            Edits : Gnatformat.Edits.Formatting_Edit_Type;

         begin
            Gnatformat.Configuration.Overwrite
              (Format_Options, CLI_Formatting_Config);

            Edits :=
              Gnatformat.Formatting.Range_Format
                (Unit            => Unit,
                 Selection_Range => Selection_Range,
                 Format_Options  => Format_Options,
                 Configuration   => Unparsing_Configuration);

            if Pipe then
               Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
               Ada.Text_IO.New_Line;
            end if;
         end;

      else
         if not Source.Is_Regular_File then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Failed to find " & Source.Display_Base_Name);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         declare
            Context : constant Libadalang.Analysis.Analysis_Context :=
              Libadalang.Analysis.Create_Context;
            Unit    : constant Libadalang.Analysis.Analysis_Unit :=
              Context.Get_From_File
                (Source.Display_Full_Name (Normalize => True),
                 Default_Charset);

            Edits : constant Gnatformat.Edits.Formatting_Edit_Type :=
              Gnatformat.Formatting.Range_Format
                (Unit            => Unit,
                 Selection_Range => Selection_Range,
                 Format_Options  => CLI_Formatting_Config,
                 Configuration   => Unparsing_Configuration);

         begin
            if Pipe then
               Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
               Ada.Text_IO.New_Line;
            end if;
         end;
      end if;
   end Range_Format;

end Gnatformat.Range_Format;
