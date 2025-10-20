--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package with the public API for editing sources

with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with GNATCOLL.VFS;

with Gnatformat.Formatting;
with Gnatformat.Helpers;
with Gnatformat.Project;

with GPR2;
with GPR2.Build.Source; use GPR2.Build.Source;

with Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers;

with Libadalang.Analysis;
with Libadalang.Preprocessing;

with Gitdiff;

package body Gnatformat.Full_Format is

   package Unbounded_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        "="          => Ada.Strings.Unbounded."=");

   subtype Unbounded_String_Vector is Unbounded_String_Vectors.Vector;

   procedure Full_Format
     (Writer                  : in out Abstract_Writers.Abstract_Writer'Class;
      Project_Tree            : GPR2.Project.Tree.Object;
      CLI_Formatting_Config   : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Configuration :
        Langkit_Support_Unparsing.Unparsing_Configuration;
      Command_Line_Sources    : Gnatformat.Command_Line.Sources.Result_Array;
      Format_Options          : Gnatformat.Configuration.Format_Options_Type;
      Check                   : Boolean;
      Keep_Going              : Boolean;
      Charset                 : String;
      Base_Commit_ID          :
        Gnatformat.Configuration.Optional_Unbounded_String)
   is
      type Preprocessor_Data_Record is record
         Preprocessor_Data : Libadalang.Preprocessing.Preprocessor_Data;
         Default_Config    : Libadalang.Preprocessing.File_Config;
         File_Configs      : Libadalang.Preprocessing.File_Config_Maps.Map;
         File_Reader       :
           Langkit_Support.File_Readers.File_Reader_Reference;
      end record;

      function Get_Preprocessor_Data return Preprocessor_Data_Record;
      --  Gets all the data needed for preprocessing

      ----------------------
      -- Get_Preprocessor --
      ----------------------

      function Get_Preprocessor_Data return Preprocessor_Data_Record is
         Result : Preprocessor_Data_Record;
      begin
         if Project_Tree.Is_Defined then
            Result.Preprocessor_Data :=
              Libadalang.Preprocessing.Extract_Preprocessor_Data_From_Project
                (Project_Tree);
            Result.Default_Config :=
              Libadalang.Preprocessing.Default_Config
                (Result.Preprocessor_Data);
            Result.File_Configs :=
              Libadalang.Preprocessing.File_Configs (Result.Preprocessor_Data);
            Result.File_Reader :=
              Libadalang.Preprocessing.Create_Preprocessor
                (Result.Default_Config, Result.File_Configs);

            --  The call above to Create_Preprocessor consumes
            --  Result.Default_Config and Result.File_Configs.
            --  However, these data strctures are useful to have.
            --  Therefore, recompute them.

            Result.Default_Config :=
              Libadalang.Preprocessing.Default_Config
                (Result.Preprocessor_Data);
            Result.File_Configs :=
              Libadalang.Preprocessing.File_Configs (Result.Preprocessor_Data);
         end if;

         return Result;
      end Get_Preprocessor_Data;

      Preprocessor_Data : constant Preprocessor_Data_Record :=
        Get_Preprocessor_Data;

      LAL_Context : constant Libadalang.Analysis.Analysis_Context :=
        Libadalang.Analysis.Create_Context
          (File_Reader => Preprocessor_Data.File_Reader);

      Project_Format_Options_Cache :
        Gnatformat.Configuration.Project_Format_Options_Cache_Type :=
          Gnatformat.Configuration.Create_Project_Format_Options_Cache;

      type Format_Source_Result (Success : Boolean) is record
         case Success is
            when True =>
               Formatted_Source : Ada.Strings.Unbounded.Unbounded_String;

            when False =>
               Diagnostics : Unbounded_String_Vector;
         end case;
      end record;

      function Format_Source
        (Source                    : GNATCOLL.VFS.Virtual_File;
         Project_Formatting_Config :
           Gnatformat.Configuration.Format_Options_Type)
         return Format_Source_Result;
      --  Resolves the right format options for Path and formats it
      --  with Project_Formatting_Config.

      function Process_Project_Source
        (Source         : Gnatformat.Project.Project_Source_Record;
         Format_Options : Gnatformat.Configuration.Format_Options_Type :=
           Gnatformat.Configuration.Default_Format_Options) return Boolean;
      --  Formats the source defined by Path.
      --  If Source is Visible, then its format options are fetched
      --  by using its owning view. If invisible, then Format_Options
      --  is used.
      --  If --pipe is used, then prints the formatted source to
      --   stdout. Otherwise writes it to disk.

      function Process_Standalone_Source
        (Source : GNATCOLL.VFS.Virtual_File; Charset : String) return Boolean;
      --  Formats the source defined by Source.
      --  If --pipe is used, then prints the formatted source to
      --  stdout. Otherwise writes it to disk.

      -------------------
      -- Format_Source --
      -------------------

      function Format_Source
        (Source                    : GNATCOLL.VFS.Virtual_File;
         Project_Formatting_Config :
           Gnatformat.Configuration.Format_Options_Type)
         return Format_Source_Result
      is
         Charset : constant String :=
           Ada.Strings.Unbounded.To_String
             (Gnatformat.Configuration.Get_Charset
                (Project_Formatting_Config, Source.Display_Base_Name));
         Unit    : constant Libadalang.Analysis.Analysis_Unit :=
           LAL_Context.Get_From_File
             (Source.Display_Full_Name (Normalize => True), Charset);

      begin
         if Unit.Has_Diagnostics then
            declare
               Diagnostics : constant Unbounded_String_Vector :=
                 [for Diagnotic of Unit.Diagnostics =>
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Langkit_Support.Diagnostics.To_Pretty_String
                         (Diagnotic))];

            begin
               return
                 Format_Source_Result'
                   (Success => False, Diagnostics => Diagnostics);
            end;
         end if;

         return
           Format_Source_Result'
             (Success          => True,
              Formatted_Source =>
                Gnatformat.Formatting.Format
                  (Unit           => Unit,
                   Format_Options => Project_Formatting_Config,
                   Configuration  => Unparsing_Configuration));
      end Format_Source;

      ----------------------------
      -- Process_Project_Source --
      ----------------------------

      function Process_Project_Source
        (Source         : Gnatformat.Project.Project_Source_Record;
         Format_Options : Gnatformat.Configuration.Format_Options_Type :=
           Gnatformat.Configuration.Default_Format_Options) return Boolean
      is
         use type Ada.Exceptions.Exception_Id;

         Source_Simple_Name : constant String := Source.File.Display_Base_Name;
         Source_Path_Name   : constant String := Source.File.Display_Full_Name;

      begin
         Gnatformat_Trace.Trace
           ("Processing project source " & Source_Simple_Name);

         if Libadalang.Preprocessing.Needs_Preprocessing
              (Preprocessor_Data.Preprocessor_Data, Source_Simple_Name)
         then
            Writer.Print_Warning
              ("--  "
               & Source_Simple_Name
               & " was skipped because it requires preprocessing");

            return True;
         end if;

         declare
            View_Format_Options :
              Gnatformat.Configuration.Format_Options_Type :=
                (case Source.Visible is
                   when True  =>
                     Gnatformat.Configuration.Get
                       (Project_Format_Options_Cache,
                        Source.Visible_Source.Owning_View),
                   when False => Format_Options);

         begin
            Gnatformat.Configuration.Overwrite
              (View_Format_Options, CLI_Formatting_Config);

            if Gnatformat.Configuration.Get_Ignore (View_Format_Options)
                 .Contains (Source_Simple_Name)
            then
               Gnatformat_Trace.Trace
                 (Source_Simple_Name
                  & " was skipped because it is in the ignore list");
               return True;
            end if;

            declare
               Result : constant Format_Source_Result :=
                 Format_Source (Source.File, View_Format_Options);

            begin
               if not Source.Visible then
                  Writer.Print_Warning
                    ("--  Warning: Formatting """
                     & Source_Path_Name
                     & """ which is not visible to the provided project");
               end if;

               case Result.Success is
                  when True  =>

                     if Check then
                        declare
                           use Ada.Strings.Unbounded;

                           Original_Source : constant Unbounded_String :=
                             Gnatformat.Helpers.Read_To_Unbounded_String
                               (Source_Path_Name);
                        begin
                           if Original_Source /= Result.Formatted_Source then
                              Gnatformat.Project.Set_General_Failed;
                              Writer.Print_Error
                                (Source_Simple_Name
                                 & " is not correctly formatted",
                                 False);
                           end if;
                        end;

                     else
                        Writer.Print_Source_Name ("--  " & Source_Simple_Name);

                        Writer.Print_Source
                          (Source_Path_Name, Result.Formatted_Source);
                     end if;

                     return True;

                  when False =>
                     Gnatformat.Project.Set_General_Failed;

                     Writer.Print_Error
                       ("--  " & Source_Simple_Name & " failed to format");

                     for Diagnostic of Result.Diagnostics loop
                        Writer.Print_Error
                          (Ada.Strings.Unbounded.To_String (Diagnostic),
                           False);
                     end loop;

                     return False;
               end case;
            end;
         end;

      exception
         when E : others =>
            Gnatformat.Project.Set_General_Failed;

            Writer.Print_Error ("Failed to format " & Source_Path_Name);
            Writer.Print_Error (Ada.Exceptions.Exception_Message (E), False);
            Gnatformat_Trace.Trace (Ada.Exceptions.Exception_Information (E));
            Gnatformat_Trace.Trace
              (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

            if Ada.Exceptions.Exception_Identity (E)
              = Gnatformat
                  .Formatting
                  .Internal_Error_Off_On_Invalid_Marker'Identity
            then
               Writer.Print_Error
                 ("This error was an internal bug, please consider "
                  & "reporting it with the --verbose traces",
                  False);
            end if;

            return False;
      end Process_Project_Source;

      -------------------------------
      -- Process_Standalone_Source --
      -------------------------------

      function Process_Standalone_Source
        (Source : GNATCOLL.VFS.Virtual_File; Charset : String) return Boolean
      is
         use type Ada.Exceptions.Exception_Id;

      begin
         Gnatformat_Trace.Trace
           ("Processing standalone source " & Source.Display_Base_Name);

         declare
            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              LAL_Context.Get_From_File (Source.Display_Full_Name, Charset);

         begin
            if Unit.Has_Diagnostics then
               Gnatformat.Project.Set_General_Failed;

               Writer.Print_Error
                 ("--  " & Source.Display_Full_Name & " failed to format");
               for Diagnostic of Unit.Diagnostics loop
                  Writer.Print_Error
                    (Langkit_Support.Diagnostics.To_Pretty_String (Diagnostic),
                     False);
               end loop;

               return False;
            end if;

            declare
               Formatted_Source :
                 constant Ada.Strings.Unbounded.Unbounded_String :=
                   Gnatformat.Formatting.Format
                     (Unit           => Unit,
                      Format_Options => Format_Options,
                      Configuration  => Unparsing_Configuration);

            begin
               if Check then
                  declare
                     Original_Source : Ada.Strings.Unbounded.Unbounded_String;

                     use type Ada.Strings.Unbounded.Unbounded_String;
                  begin
                     Original_Source :=
                       Gnatformat.Helpers.Read_To_Unbounded_String
                         (Source.Display_Full_Name);

                     if Original_Source /= Formatted_Source then
                        Gnatformat.Project.Set_General_Failed;
                        Writer.Print_Error
                          (Source.Display_Full_Name
                           & " is not correctly formatted",
                           False);
                     end if;
                  end;

               else
                  Writer.Print_Source_Name ("--  " & Source.Display_Base_Name);

                  Writer.Print_Source
                    (Source.Display_Full_Name, Formatted_Source);
               end if;
            end;

            return True;
         end;

      exception
         when E : others =>
            Gnatformat.Project.Set_General_Failed;

            Writer.Print_Error
              ("Failed to format " & Source.Display_Full_Name);
            Writer.Print_Error (Ada.Exceptions.Exception_Message (E), False);
            Gnatformat_Trace.Trace (Ada.Exceptions.Exception_Information (E));
            Gnatformat_Trace.Trace
              (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

            if Ada.Exceptions.Exception_Identity (E)
              = Gnatformat
                  .Formatting
                  .Internal_Error_Off_On_Invalid_Marker'Identity
            then
               Writer.Print_Error
                 ("This error was an internal bug, please consider "
                  & "reporting it with the --verbose traces",
                  False);
            end if;

            return False;
      end Process_Standalone_Source;

   begin
      if Preprocessor_Data.Default_Config.Enabled then
         --  If Preprocessor_Data.Default_Config is enabled, then all
         --  sources need to be preprocessed by Libadalang, even if
         --  they do not have preprocessing directives.
         --  Libadalang.Preprocessing.Needs_Preprocessing will return
         --  True for any source, therefore, we cannot differentiate
         --  between sources that require preprocessing and not.
         --  Therefore, do not format anything and suggest the usage
         --  of '-gnatep'

         Writer.Print_Error
           ("Projects with global preprocessor symbols are not "
            & "supported. Please use the -gnatep switch with a "
            & "proprocessor data file.",
            False);
         Gnatformat.Project.Set_General_Failed;

      elsif Command_Line_Sources'Length > 0 then
         --  Only print the source simple name as a comment if there's
         --- more than one source to format.

         if Project_Tree.Is_Defined then
            declare
               use Gnatformat.Project;
               Command_Line_Project_Sources :
                 constant Gnatformat.Project.Project_Source_Vector :=
                   To_Project_Sources (Project_Tree, Command_Line_Sources);

               Format_Options : Gnatformat.Configuration.Format_Options_Type :=
                 Gnatformat.Configuration.Get
                   (Project_Format_Options_Cache, Project_Tree.Root_Project);

            begin
               Gnatformat.Configuration.Overwrite
                 (Format_Options, CLI_Formatting_Config);

               for Source of Command_Line_Project_Sources loop
                  exit when
                    not Process_Project_Source (Source, Format_Options)
                    and not Keep_Going;
               end loop;
            end;

         else
            for Source of Command_Line_Sources loop
               if not Source.Is_Regular_File then
                  Gnatformat.Project.Set_General_Failed;

                  Writer.Print_Error
                    ("Failed to find " & Source.Display_Base_Name);

                  if not Keep_Going then
                     exit;
                  end if;

               else
                  exit when
                    not Process_Standalone_Source (Source, Charset)
                    and not Keep_Going;
               end if;
            end loop;
         end if;

      elsif Base_Commit_ID.Is_Set then
         begin
            Gitdiff.Format_New_Lines
              (Ada.Strings.Unbounded.To_String (Base_Commit_ID.Value),
               Gitdiff.Context'
                 (Lal_Ctx          => LAL_Context,
                  Options          => Format_Options,
                  Unparsing_Config => Unparsing_Configuration,
                  Charset          =>
                    Ada.Strings.Unbounded.To_Unbounded_String (Charset)));
         exception
            when E : others =>
               Writer.Print_Error
                 ("Failed to generate git diff: make sure to specify a valid commit ID",
                  True);
               Gnatformat.Project.Set_General_Failed;
         end;

      else
         declare
            use Gnatformat.Project;
            Project_Sources : constant Project_Source_Vector :=
              [for Source of Get_Project_Sources (Project_Tree) =>
                 Project_Source_Record'
                   (File           =>
                      GNATCOLL.VFS.Create_From_UTF8
                        (Source.Path_Name.String_Value),
                    Visible        => True,
                    Visible_Source => Source)];

         begin
            for Source of Project_Sources loop
               exit when
                 not Process_Project_Source (Source) and not Keep_Going;
            end loop;
         end;
      end if;

      if Gnatformat.Project.General_Failed then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Full_Format;

end Gnatformat.Full_Format;
