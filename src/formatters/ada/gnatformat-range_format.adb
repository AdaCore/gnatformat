--
--  Copyright (C) 2025-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Gnatformat.Bail;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Console_Writers;
with Gnatformat.Encodings;
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

   use type GNATCOLL.VFS.Virtual_File;

   package Encodings renames Gnatformat.Encodings;

   package Langkit_Support_Unparsing renames
     Langkit_Support.Generic_API.Unparsing;

   function Casing_Normalized_Unit
     (Resolution_Context : Libadalang.Analysis.Analysis_Context;
      Filename           : String;
      Charset            : String;
      Casing             :
        Gnatformat.Configuration.Normalizing_Identifier_Casing_Kind)
      return Libadalang.Analysis.Analysis_Unit;
   --  Parses Filename in Resolution_Context and returns its identifier-casing
   --  normalized form (see Gnatformat.Identifier_Casing.Normalized_Unit).
   --  Falls back to the parsed unit when it has diagnostics.

   ----------------------------
   -- Casing_Normalized_Unit --
   ----------------------------

   function Casing_Normalized_Unit
     (Resolution_Context : Libadalang.Analysis.Analysis_Context;
      Filename           : String;
      Charset            : String;
      Casing             :
        Gnatformat.Configuration.Normalizing_Identifier_Casing_Kind)
      return Libadalang.Analysis.Analysis_Unit
   is
      Original_Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Resolution_Context.Get_From_File (Filename, Charset);

   begin
      if Original_Unit.Has_Diagnostics then
         return Original_Unit;
      end if;

      return
        Gnatformat.Identifier_Casing.Normalized_Unit (Original_Unit, Casing);
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
      Format_Options               :
        Gnatformat.Configuration.Format_Options_Type)
   is
      --  An explicit base path must reach the loader even if it is missing.
      --  Falling back here would silently ignore the requested configuration.
      Unparsing_Configuration_Cache :
        Gnatformat.Configuration.Unparsing_Configuration_Cache_Type :=
          (if Unparsing_Configuration_File /= GNATCOLL.VFS.No_File
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

      function Get_Unparsing_Configuration
        (Options : Gnatformat.Configuration.Format_Options_Type)
         return Langkit_Support_Unparsing.Unparsing_Configuration;
      --  Report loading errors and stop before computing any edits.

      ---------------------------------
      -- Get_Unparsing_Configuration --
      ---------------------------------

      function Get_Unparsing_Configuration
        (Options : Gnatformat.Configuration.Format_Options_Type)
         return Langkit_Support_Unparsing.Unparsing_Configuration
      is
         use type Langkit_Support_Unparsing.Unparsing_Configuration;

         Writer : Console_Writers.Console_Writer :=
           Console_Writers.Writer (Single_File => True);
         Config : constant Langkit_Support_Unparsing.Unparsing_Configuration :=
           Command_Line.Configuration.Get_Unparsing_Configuration
             (Unparsing_Configuration_Cache,
              Source.Display_Base_Name,
              Options,
              Writer);

      begin
         --  This function is called while initializing the declarations below.
         --  Stop here so neither range-format branch computes an edit with
         --  an invalid configuration or prints a misleading empty result.
         if Config = Langkit_Support_Unparsing.No_Unparsing_Configuration then
            Gnatformat.Bail.Bail (1);
         end if;
         return Config;
      end Get_Unparsing_Configuration;

      procedure Check_Diagnostics
        (Edit : Gnatformat.Edits.Formatting_Edit_Type);
      --  Report library diagnostics instead of printing an empty edit

      -----------------------
      -- Check_Diagnostics --
      -----------------------

      procedure Check_Diagnostics
        (Edit : Gnatformat.Edits.Formatting_Edit_Type) is
      begin
         if not Edit.Diagnostics.Is_Empty then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "--  " & Source.Display_Base_Name & " failed to format");
            for Diagnostic of Edit.Diagnostics loop
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Langkit_Support.Diagnostics.To_Pretty_String (Diagnostic));
            end loop;
            Gnatformat.Bail.Bail (1);
         end if;
      end Check_Diagnostics;

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

            View_Format_Options :
              Gnatformat.Configuration.Format_Options_Type :=
                (case Project_Source.Visible is
                   when True  =>
                     Gnatformat.Configuration.Get
                       (Project_Format_Options_Cache,
                        Project_Source.Visible_Source.Owning_View),
                   when False =>
                     Gnatformat.Configuration.Get
                       (Project_Format_Options_Cache,
                        Project_Tree.Root_Project));

            Source_Path : constant String :=
              Project_Source.File.Display_Full_Name (Normalize => True);

         begin
            Gnatformat.Configuration.Overwrite
              (View_Format_Options, CLI_Formatting_Config);

            declare
               Encoding : constant Encodings.Source_Encoding :=
                 Encodings.Resolve_Encoding
                   (View_Format_Options, Project_Source.File);

               Parsing_Charset : constant String :=
                 Encodings.Parsing_Charset (Encoding);

               Identifier_Casing :
                 constant Gnatformat.Configuration.Identifier_Casing_Kind :=
                   Gnatformat.Configuration.Get_Identifier_Casing
                     (View_Format_Options,
                      Project_Source.File.Display_Base_Name);

               Resolution_Context :
                 constant Libadalang.Analysis.Analysis_Context :=
                   (case Identifier_Casing is
                      when Gnatformat.Configuration.Definition =>
                        Gnatformat.Project.Create_Resolution_Context
                          (Project_Tree),
                      when Gnatformat.Configuration.Keep
                         | Gnatformat.Configuration.Lower
                         | Gnatformat.Configuration.Upper
                         | Gnatformat.Configuration.Mixed      =>
                        Libadalang.Analysis.Create_Context);

               Unit : constant Libadalang.Analysis.Analysis_Unit :=
                 (case Identifier_Casing is
                    when Gnatformat.Configuration.Keep  =>
                      Resolution_Context.Get_From_File
                        (Source_Path, Parsing_Charset),
                    when Gnatformat.Configuration.Definition
                       | Gnatformat.Configuration.Lower
                       | Gnatformat.Configuration.Upper
                       | Gnatformat.Configuration.Mixed =>
                      Casing_Normalized_Unit
                        (Resolution_Context => Resolution_Context,
                         Filename           => Source_Path,
                         Charset            => Parsing_Charset,
                         Casing             => Identifier_Casing));

               Unparsing_Config :
                 constant Langkit_Support_Unparsing.Unparsing_Configuration :=
                   Get_Unparsing_Configuration (View_Format_Options);

               Edits : Gnatformat.Edits.Formatting_Edit_Type :=
                 Gnatformat.Formatting.Range_Format
                   (Unit            => Unit,
                    Selection_Range => Selection_Range,
                    Format_Options  => View_Format_Options,
                    Configuration   => Unparsing_Config);

            begin
               Check_Diagnostics (Edits);

               --  The formatted text is UTF-8 encoded. Encode it back into
               --  the same charset used to decode the source so that the
               --  edit keeps the input encoding.

               Edits.Text_Edit.Text :=
                 Encodings.Encode
                   (Edits.Text_Edit.Text,
                    Ada.Strings.Unbounded.To_String (Encoding.Charset));

               Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
               Ada.Text_IO.New_Line;
            end;
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

            Encoding : constant Encodings.Source_Encoding :=
              Encodings.Resolve_Encoding (Format_Options, Source);

            Parsing_Charset : constant String :=
              Encodings.Parsing_Charset (Encoding);

            Identifier_Casing :
              constant Gnatformat.Configuration.Identifier_Casing_Kind :=
                Gnatformat.Configuration.Get_Identifier_Casing
                  (Format_Options, Source.Display_Base_Name);

            Resolution_Context :
              constant Libadalang.Analysis.Analysis_Context :=
                Libadalang.Analysis.Create_Context;

            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              (case Identifier_Casing is
                 when Gnatformat.Configuration.Keep  =>
                   Resolution_Context.Get_From_File
                     (Source_Path, Parsing_Charset),
                 when Gnatformat.Configuration.Definition
                    | Gnatformat.Configuration.Lower
                    | Gnatformat.Configuration.Upper
                    | Gnatformat.Configuration.Mixed =>
                   Casing_Normalized_Unit
                     (Resolution_Context => Resolution_Context,
                      Filename           => Source_Path,
                      Charset            => Parsing_Charset,
                      Casing             => Identifier_Casing));

            Unparsing_Config :
              constant Langkit_Support_Unparsing.Unparsing_Configuration :=
                Get_Unparsing_Configuration (Format_Options);

            Edits : Gnatformat.Edits.Formatting_Edit_Type :=
              Gnatformat.Formatting.Range_Format
                (Unit            => Unit,
                 Selection_Range => Selection_Range,
                 Format_Options  => Format_Options,
                 Configuration   => Unparsing_Config);

         begin
            Check_Diagnostics (Edits);

            --  The formatted text is UTF-8 encoded. Encode it back into the
            --  same charset used to decode the source so that the edit keeps
            --  the input encoding.

            Edits.Text_Edit.Text :=
              Encodings.Encode
                (Edits.Text_Edit.Text,
                 Ada.Strings.Unbounded.To_String (Encoding.Charset));

            Ada.Text_IO.Put_Line (Gnatformat.Edits.Image (Edits));
            Ada.Text_IO.New_Line;
         end;
      end if;

   exception
      when E : Encodings.Undetermined_Encoding_Error =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Source.Display_Base_Name
            & " cannot be formatted because "
            & Ada.Exceptions.Exception_Message (E)
            & ". Use --charset to format it.");
         Gnatformat.Bail.Bail (1);

      when E : Encodings.Incompatible_BOM_Error =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Source.Display_Base_Name & " cannot be formatted.");
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Message (E));
         Gnatformat.Bail.Bail (1);
   end Range_Format;

end Gnatformat.Range_Format;
