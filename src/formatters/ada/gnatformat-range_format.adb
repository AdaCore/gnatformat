--
--  Copyright (C) 2025-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Gnatformat.Bail;
with Gnatformat.Formatting;
with Gnatformat.Edits;
with Gnatformat.Identifier_Casing;
with Gnatformat.Project;

with GPR2;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;
with Libadalang.Generic_API.Unparsing;

package body Gnatformat.Range_Format is

   package Langkit_Support_Unparsing renames
     Langkit_Support.Generic_API.Unparsing;

   function Casing_Normalized_Unit
     (Resolution_Context : Libadalang.Analysis.Analysis_Context;
      Filename           : String;
      Charset            : String) return Libadalang.Analysis.Analysis_Unit;
   --  Parses Filename in Resolution_Context and returns its identifier-casing
   --  normalized form (see Gnatformat.Identifier_Casing.Normalized_Unit).
   --  Falls back to the parsed unit when it has diagnostics.

   ----------------------------
   -- Casing_Normalized_Unit --
   ----------------------------

   function Casing_Normalized_Unit
     (Resolution_Context : Libadalang.Analysis.Analysis_Context;
      Filename           : String;
      Charset            : String) return Libadalang.Analysis.Analysis_Unit
   is
      Original_Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Resolution_Context.Get_From_File (Filename, Charset);

   begin
      if Original_Unit.Has_Diagnostics then
         return Original_Unit;
      end if;

      return Gnatformat.Identifier_Casing.Normalized_Unit (Original_Unit);
   end Casing_Normalized_Unit;

   --------------------
   --  Range_Format  --
   --------------------

   procedure Range_Format
     (Project_Tree                 : GPR2.Project.Tree.Object;
      Source                       : GNATCOLL.VFS.Virtual_File;
      Selection_Range              :
        Langkit_Support.Slocs.Source_Location_Range;
      CLI_Formatting_Config        :
        Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Configuration_File : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      Default_Charset              : String :=
        Gnatformat.Configuration.Default_Charset;
      Pipe                         : Boolean := False)
   is
      Unparsing_Configuration_Cache :
        Gnatformat.Configuration.Unparsing_Configuration_Cache_Type :=
          (if Unparsing_Configuration_File.Is_Regular_File
           then
             Gnatformat.Configuration.Create_Unparsing_Configuration_Cache
               (Unparsing_Configuration_File)
           else
             Gnatformat.Configuration.Create_Unparsing_Configuration_Cache
               (GNATCOLL.VFS.Create_From_UTF8
                  (Libadalang
                     .Generic_API
                     .Unparsing
                     .Default_Configuration_Filename)));

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
              constant Gnatformat.Configuration.Format_Options_Type :=
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

            Source_Path : constant String :=
              Project_Source.File.Display_Full_Name (Normalize => True);

            Identifier_Casing :
              constant Gnatformat.Configuration.Identifier_Casing_Kind :=
                Gnatformat.Configuration.Get_Identifier_Casing
                  (Project_Formatting_Config,
                   Project_Source.File.Display_Base_Name);

            Resolution_Context :
              constant Libadalang.Analysis.Analysis_Context :=
                (case Identifier_Casing is
                   when Gnatformat.Configuration.Definition =>
                     Gnatformat.Project.Create_Resolution_Context
                       (Project_Tree),
                   when Gnatformat.Configuration.Keep       =>
                     Libadalang.Analysis.Create_Context);

            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              (case Identifier_Casing is
                 when Gnatformat.Configuration.Definition =>
                   Casing_Normalized_Unit
                     (Resolution_Context => Resolution_Context,
                      Filename           => Source_Path,
                      Charset            => Charset),
                 when Gnatformat.Configuration.Keep       =>
                   Resolution_Context.Get_From_File (Source_Path, Charset));

            Edits : Gnatformat.Edits.Formatting_Edit_Type;

         begin
            Gnatformat.Configuration.Overwrite
              (Format_Options, CLI_Formatting_Config);

            declare
               Unparsing_Diagnostics :
                 Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
               Unparsing_Config      :
                 constant Langkit_Support_Unparsing.Unparsing_Configuration :=
                   (case Project_Source.Visible is
                      when True  =>
                        Gnatformat.Configuration.Get
                          (Unparsing_Configuration_Cache,
                           Project_Source.File.Display_Base_Name,
                           Project_Source.Visible_Source.Owning_View,
                           Format_Options,
                           Unparsing_Diagnostics),
                      when False =>
                        Gnatformat.Configuration.Get
                          (Unparsing_Configuration_Cache,
                           Project_Source.File.Display_Base_Name,
                           Format_Options,
                           Unparsing_Diagnostics));

            begin
               Edits :=
                 Gnatformat.Formatting.Range_Format
                   (Unit            => Unit,
                    Selection_Range => Selection_Range,
                    Format_Options  => Format_Options,
                    Configuration   => Unparsing_Config);
            end;

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
            Gnatformat.Bail.Bail (1);
         end if;

         declare
            Source_Path : constant String :=
              Source.Display_Full_Name (Normalize => True);

            Identifier_Casing :
              constant Gnatformat.Configuration.Identifier_Casing_Kind :=
                Gnatformat.Configuration.Get_Identifier_Casing
                  (CLI_Formatting_Config, Source.Display_Base_Name);

            Resolution_Context :
              constant Libadalang.Analysis.Analysis_Context :=
                Libadalang.Analysis.Create_Context;

            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              (case Identifier_Casing is
                 when Gnatformat.Configuration.Definition =>
                   Casing_Normalized_Unit
                     (Resolution_Context => Resolution_Context,
                      Filename           => Source_Path,
                      Charset            => Default_Charset),
                 when Gnatformat.Configuration.Keep       =>
                   Resolution_Context.Get_From_File
                     (Source_Path, Default_Charset));

            Unparsing_Diagnostics :
              Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
            Unparsing_Config      :
              constant Langkit_Support_Unparsing.Unparsing_Configuration :=
                Gnatformat.Configuration.Get
                  (Unparsing_Configuration_Cache,
                   Source.Display_Base_Name,
                   CLI_Formatting_Config,
                   Unparsing_Diagnostics);

            Edits : constant Gnatformat.Edits.Formatting_Edit_Type :=
              Gnatformat.Formatting.Range_Format
                (Unit            => Unit,
                 Selection_Range => Selection_Range,
                 Format_Options  => CLI_Formatting_Config,
                 Configuration   => Unparsing_Config);

         begin
            if Pipe then
               Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
               Ada.Text_IO.New_Line;
            end if;
         end;
      end if;
   end Range_Format;

end Gnatformat.Range_Format;
