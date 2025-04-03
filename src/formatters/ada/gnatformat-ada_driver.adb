--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
pragma Warnings (Off, "-gnatwi");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "-gnatwi");
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gnatformat.Command_Line;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Configuration;
with Gnatformat.Formatting;
with Gnatformat.Helpers;
with Gnatformat.Range_Format;
with Gnatformat.Project;

with GPR2;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;

with Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;
with Libadalang.Preprocessing;

with Gitdiff;

procedure Gnatformat.Ada_Driver is

   package Langkit_Support_Unparsing
     renames Langkit_Support.Generic_API.Unparsing;

   type Preprocessor_Data_Record is record
      Preprocessor_Data : Libadalang.Preprocessing.Preprocessor_Data;
      Default_Config    : Libadalang.Preprocessing.File_Config;
      File_Configs      : Libadalang.Preprocessing.File_Config_Maps.Map;
      File_Reader       : Langkit_Support.File_Readers.File_Reader_Reference;
   end record;

   package Unbounded_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        "="          => Ada.Strings.Unbounded."=");

   subtype Unbounded_String_Vector is Unbounded_String_Vectors.Vector;

begin
   GNATCOLL.Traces.Parse_Config_File;

   declare
      Unparsed_Arguments : GNATCOLL.Opt_Parse.XString_Vector;
   begin
      if not Gnatformat.Command_Line.Parser.Parse
               (Unknown_Arguments => Unparsed_Arguments)
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "Failed to parse CLI arguments");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if not Gnatformat.Command_Line.GPR_Args.Parse_GPR2_Options
        (Arguments => Unparsed_Arguments,
         Options   => Gnatformat.Project.GPR_Options)
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "Failed to parse CLI GPR arguments");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end;

   if not Gnatformat.Command_Line.Parser.Parse then
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

   Gnatformat.Project.GPR_Options.Print_GPR_Registry;

   if Gnatformat.Command_Line.Range_Format.Get then
      --  Gnatformat range formatting mode is activated
      Gnatformat.Range_Format.Proceed_With_Range_Formatting
        (Gnatformat.Project.Resolve_Project_File);
   else
      --  GNATformat normal driver behavior
      declare
         use type Gnatformat.Command_Line.Sources.Result_Array;
         use type Langkit_Support_Unparsing.Unparsing_Configuration;
         use Langkit_Support.Diagnostics;

         Project_File : constant GNATCOLL.VFS.Virtual_File :=
           Gnatformat.Project.Resolve_Project_File;
         Project_Tree : GPR2.Project.Tree.Object;

         CLI_Formatting_Config :
         constant Gnatformat.Configuration.Format_Options_Type :=
           Gnatformat.Command_Line.Configuration.Get;

         Diagnostics : Diagnostics_Vectors.Vector;

         Unparsing_Configuration_File :
         constant GNATCOLL.VFS.Virtual_File :=
           Gnatformat.Command_Line.Unparsing_Configuration.Get;
         Unparsing_Configuration      :
         constant Langkit_Support_Unparsing.Unparsing_Configuration :=
           Gnatformat.Configuration.Load_Unparsing_Configuration
             (Unparsing_Configuration_File, Diagnostics);

      begin
         if Project_File = GNATCOLL.VFS.No_File
           and Gnatformat.Command_Line.Sources.Get
             = Gnatformat.Command_Line.Sources.No_Results
           and not Gnatformat.Command_Line.Gitdiff.Get.Is_Set
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Either a project or sources must be provided.");
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               GNATCOLL.Opt_Parse.Help (Gnatformat.Command_Line.Parser));
            GNAT.OS_Lib.OS_Exit (1);
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
              (Ada.Text_IO.Standard_Error,
               "Failed to load formatting rules");
            for Diagnostic of Diagnostics loop
               Gnatformat.Gnatformat_Trace.Trace
                 (Langkit_Support
                  .Diagnostics
                  .To_Pretty_String (Diagnostic));
            end loop;
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         if Project_File /= GNATCOLL.VFS.No_File then
            Gnatformat.Project.Load_Project (Project_Tree, Project_File);
         end if;

         declare
            function Get_Preprocessor_Data return Preprocessor_Data_Record;
            --  Gets all the data needed for preprocessing

            ----------------------
            -- Get_Preprocessor --
            ----------------------

            function Get_Preprocessor_Data return Preprocessor_Data_Record
            is
               Result : Preprocessor_Data_Record;
            begin
               if Project_Tree.Is_Defined then
                  Result.Preprocessor_Data :=
                    Libadalang
                      .Preprocessing
                        .Extract_Preprocessor_Data_From_Project (Project_Tree);
                  Result.Default_Config :=
                    Libadalang.Preprocessing.Default_Config
                      (Result.Preprocessor_Data);
                  Result.File_Configs :=
                    Libadalang.Preprocessing.File_Configs
                      (Result.Preprocessor_Data);
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
                    Libadalang.Preprocessing.File_Configs
                      (Result.Preprocessor_Data);
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

            Print_Source_Simple_Name : Boolean := True;
            Print_New_Line           : Boolean := False;

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
               Format_Options :
               Gnatformat.Configuration.Format_Options_Type :=
                 Gnatformat.Configuration.Default_Format_Options)
                  return Boolean;
            --  Formats the source defined by Path.
            --  If Source is Visible, then its format options are fetched
            --  by using its owning view. If invisible, then Format_Options
            --  is used.
            --  If --pipe is used, then prints the formatted source to
            --   stdout. Otherwise writes it to disk.

            function Process_Standalone_Source
              (Source : GNATCOLL.VFS.Virtual_File; Charset : String)
                  return Boolean;
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
                      (Project_Formatting_Config,
                       Source.Display_Base_Name));
               Unit    : constant Libadalang.Analysis.Analysis_Unit :=
                 LAL_Context.Get_From_File
                   (Source.Display_Full_Name (Normalize => True),
                    Charset);

            begin
               if Unit.Has_Diagnostics then
                  declare
                     Diagnostics : constant Unbounded_String_Vector :=
                       [for Diagnotic of Unit.Diagnostics
                        => Ada.Strings.Unbounded.To_Unbounded_String
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
               Format_Options :
               Gnatformat.Configuration.Format_Options_Type :=
                 Gnatformat.Configuration.Default_Format_Options)
                  return Boolean
            is
               use type Ada.Exceptions.Exception_Id;

               Source_Simple_Name : constant String :=
                 Source.File.Display_Base_Name;
               Source_Path_Name   : constant String :=
                 Source.File.Display_Full_Name;

            begin
               Gnatformat_Trace.Trace
                 ("Processing project source " & Source_Simple_Name);

               if Libadalang.Preprocessing.Needs_Preprocessing
                 (Preprocessor_Data.Preprocessor_Data, Source_Simple_Name)
               then
                  if Print_New_Line then
                     Ada.Text_IO.New_Line;
                  else
                     Print_New_Line := True;
                  end if;

                  Ada.Text_IO.Put_Line
                    ("--  "
                     & Source_Simple_Name
                     & " was skipped because it requires preprocessing");

                  return True;
               end if;

               declare
                  View_Format_Options :
                  Gnatformat.Configuration.Format_Options_Type :=
                    (case Source.Visible is
                        when True =>
                          Gnatformat.Configuration.Get
                       (Project_Format_Options_Cache,
                        Source.Visible_Source.Owning_View),
                        when False => Format_Options);

               begin
                  Gnatformat.Configuration.Overwrite
                    (View_Format_Options, CLI_Formatting_Config);

                  if Gnatformat
                    .Configuration
                      .Get_Ignore (View_Format_Options)
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
                     if Gnatformat.Command_Line.Pipe.Get then
                        case Result.Success is
                           when True =>
                              if Print_New_Line then
                                 Ada.Text_IO.New_Line;
                              else
                                 Print_New_Line := True;
                              end if;

                              if not Source.Visible then
                                 Ada.Text_IO.Put_Line
                                   ("--  Warning: Formatting """
                                    & Source_Path_Name
                                    & """ which is not visible to the provided"
                                    & " project");
                              end if;
                              if Print_Source_Simple_Name then
                                 Ada.Text_IO.Put_Line
                                   ("--  " & Source_Simple_Name);
                              end if;

                              Ada.Strings.Unbounded.Text_IO.Put
                                (Result.Formatted_Source);

                           when False =>
                              Gnatformat.Project.Set_General_Failed;

                              if Print_New_Line then
                                 Ada.Text_IO.New_Line
                                   (Ada.Text_IO.Standard_Error);
                              else
                                 Print_New_Line := True;
                              end if;

                              Ada.Text_IO.Put_Line
                                (Ada.Text_IO.Standard_Error,
                                 "--  "
                                 & Source_Simple_Name
                                 & " failed to format");
                              for Diagnostic of Result.Diagnostics loop
                                 Ada.Strings.Unbounded.Text_IO.Put_Line
                                   (Ada.Text_IO.Standard_Error, Diagnostic);
                              end loop;

                              return False;
                        end case;

                     else
                        if not Source.Visible then
                           Ada.Text_IO.Put_Line
                             ("Warning: Formatting """
                              & Source_Path_Name
                              & """ which is not visible to the provided "
                              & "project");
                        end if;
                        if Gnatformat.Command_Line.Check.Get then
                           declare
                              use Ada.Strings.Unbounded;

                              Original_Source : constant Unbounded_String :=
                                Gnatformat.Helpers.Read_To_Unbounded_String
                                  (Source_Path_Name);
                           begin
                              if Original_Source /= Result.Formatted_Source
                              then
                                 Gnatformat.Project.Set_General_Failed;
                                 Ada.Text_IO.Put_Line
                                   (Ada.Text_IO.Standard_Error,
                                    Source_Simple_Name
                                    & " is not correctly formatted");
                              end if;
                           end;

                        else
                           Gnatformat.Helpers.Write
                             (Source_Path_Name, Result.Formatted_Source);
                        end if;
                     end if;
                  end;

                  return True;
               end;

            exception
               when E : others =>
                  Gnatformat.Project.Set_General_Failed;

                  if Print_New_Line then
                     Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
                  else
                     Print_New_Line := True;
                  end if;

                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "Failed to format " & Source_Path_Name);
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Ada.Exceptions.Exception_Message (E));
                  Gnatformat_Trace.Trace
                    (Ada.Exceptions.Exception_Information (E));
                  Gnatformat_Trace.Trace
                    (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

                  if Ada.Exceptions.Exception_Identity (E)
                    = Gnatformat
                    .Formatting
                      .Internal_Error_Off_On_Invalid_Marker'Identity
                  then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "This error was an internal bug, please consider "
                        & "reporting it with the --verbose traces");
                  end if;

                  return False;
            end Process_Project_Source;

            -------------------------------
            -- Process_Standalone_Source --
            -------------------------------

            function Process_Standalone_Source
              (Source : GNATCOLL.VFS.Virtual_File; Charset : String)
                  return Boolean
            is
               use type Ada.Exceptions.Exception_Id;

            begin
               Gnatformat_Trace.Trace
                 ("Processing standalone source "
                  & Source.Display_Base_Name);

               declare
                  Unit : constant Libadalang.Analysis.Analysis_Unit :=
                    LAL_Context.Get_From_File (Source.Display_Full_Name,
                                               Charset);

               begin
                  if Unit.Has_Diagnostics then
                     Gnatformat.Project.Set_General_Failed;

                     if Print_New_Line then
                        Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
                     else
                        Print_New_Line := True;
                     end if;

                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "--  "
                        & Source.Display_Full_Name
                        & " failed to format");
                     for Diagnostic of Unit.Diagnostics loop
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           Langkit_Support.Diagnostics.To_Pretty_String
                             (Diagnostic));
                     end loop;

                     return False;
                  end if;

                  if Gnatformat.Command_Line.Pipe.Get then
                     declare
                        Formatted_Source :
                        constant Ada.Strings.Unbounded.Unbounded_String :=
                          Gnatformat.Formatting.Format
                            (Unit           => Unit,
                             Format_Options =>
                               Gnatformat.Command_Line.Configuration.Get,
                             Configuration  => Unparsing_Configuration);

                     begin
                        if Print_New_Line then
                           Ada.Text_IO.New_Line;
                        else
                           Print_New_Line := True;
                        end if;

                        if Print_Source_Simple_Name then
                           Ada.Text_IO.Put_Line
                             ("--  " & Source.Display_Base_Name);
                        end if;

                        Ada.Strings.Unbounded.Text_IO.Put
                          (Formatted_Source);
                     end;

                  else
                     declare
                        Formatted_Source        :
                        constant Ada.Strings.Unbounded.Unbounded_String :=
                          Gnatformat.Formatting.Format
                            (Unit           => Unit,
                             Format_Options =>
                               Gnatformat.Command_Line.Configuration.Get,
                             Configuration  => Unparsing_Configuration);
                        Formatted_Source_Access :
                        Ada.Strings.Unbounded.Aux.Big_String_Access;
                        Formatted_Source_Length : Natural;

                        Source_File   : Ada.Streams.Stream_IO.File_Type;
                        Source_Stream : Ada.Streams.Stream_IO.Stream_Access;

                     begin
                        if Gnatformat.Command_Line.Check.Get then
                           declare
                              Original_Source_Size :
                              constant Ada.Directories.File_Size :=
                                Ada.Directories.Size (Source
                                                      .Display_Full_Name);
                              Original_Source      :
                              Ada.Strings.Unbounded.String_Access :=
                                new String
                                  (1 .. Integer (Original_Source_Size));

                              use type Ada
                                .Strings
                                  .Unbounded
                                    .Unbounded_String;
                           begin
                              Ada.Streams.Stream_IO.Open
                                (File => Source_File,
                                 Mode => Ada.Streams.Stream_IO.In_File,
                                 Name => Source.Display_Full_Name);

                              Source_Stream :=
                                Ada.Streams.Stream_IO.Stream (Source_File);

                              String'Read (Source_Stream,
                                           Original_Source.all);

                              Ada.Streams.Stream_IO.Close (Source_File);

                              if Original_Source.all /= Formatted_Source
                              then
                                 Gnatformat.Project.Set_General_Failed;
                                 Ada.Text_IO.Put_Line
                                   (Ada.Text_IO.Standard_Error,
                                    Source.Display_Full_Name
                                    & " is not correctly formatted");
                              end if;

                              Ada.Strings.Unbounded.Free (Original_Source);
                           end;

                        else
                           Ada.Strings.Unbounded.Aux.Get_String
                             (Formatted_Source,
                              Formatted_Source_Access,
                              Formatted_Source_Length);

                           Ada.Streams.Stream_IO.Create
                             (File => Source_File,
                              Mode => Ada.Streams.Stream_IO.Out_File,
                              Name => Source.Display_Full_Name);

                           Source_Stream :=
                             Ada.Streams.Stream_IO.Stream (Source_File);

                           String'Write
                             (Source_Stream,
                              Formatted_Source_Access.all
                                (1 .. Formatted_Source_Length));

                           Ada.Streams.Stream_IO.Close (Source_File);
                        end if;
                     end;
                  end if;

                  return True;
               end;

            exception
               when E : others =>
                  Gnatformat.Project.Set_General_Failed;

                  if Print_New_Line then
                     Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
                  else
                     Print_New_Line := True;
                  end if;

                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "Failed to format " & Source.Display_Full_Name);
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Ada.Exceptions.Exception_Message (E));
                  Gnatformat_Trace.Trace
                    (Ada.Exceptions.Exception_Information (E));
                  Gnatformat_Trace.Trace
                    (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

                  if Ada.Exceptions.Exception_Identity (E)
                    = Gnatformat
                    .Formatting
                      .Internal_Error_Off_On_Invalid_Marker'Identity
                  then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "This error was an internal bug, please consider "
                        & "reporting it with the --verbose traces");
                  end if;

                  return False;
            end Process_Standalone_Source;

            Base_Commit_ID :
            constant Gnatformat.Configuration.Optional_Unbounded_String :=
              Gnatformat.Command_Line.Gitdiff.Get;

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

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Projects with global preprocessor symbols are not "
                  & "supported. Please use the -gnatep switch with a "
                  & "proprocessor data file.");
               Gnatformat.Project.Set_General_Failed;

            elsif Gnatformat.Command_Line.Sources.Get'Length > 0 then
               --  Only print the source simple name as a comment if there's
               --- more than one source to format.

               Print_Source_Simple_Name :=
                 Gnatformat.Command_Line.Sources.Get'Length > 1;

               if Project_Tree.Is_Defined then
                  declare
                     use Gnatformat.Project;
                     Command_Line_Sources :
                     constant Gnatformat.Project.Project_Source_Vector :=
                       Get_Command_Line_Sources (Project_Tree);

                     Format_Options :
                     Gnatformat.Configuration.Format_Options_Type :=
                       Gnatformat.Configuration.Get
                         (Project_Format_Options_Cache,
                          Project_Tree.Root_Project);

                  begin
                     Gnatformat.Configuration.Overwrite
                       (Format_Options, CLI_Formatting_Config);

                     for Source of Command_Line_Sources loop
                        exit when
                        not Process_Project_Source (Source, Format_Options)
                          and not Gnatformat.Command_Line.Keep_Going.Get;
                     end loop;
                  end;

               else
                  declare
                     Charset :
                     constant Gnatformat
                       .Configuration
                         .Optional_Unbounded_String :=
                           Gnatformat.Command_Line.Charset.Get;
                  begin
                     for Source of Gnatformat.Command_Line.Sources.Get loop
                        if not Source.Is_Regular_File then
                           Gnatformat.Project.Set_General_Failed;

                           if Print_New_Line then
                              Ada.Text_IO.New_Line
                                (Ada.Text_IO.Standard_Error);
                           else
                              Print_New_Line := True;
                           end if;

                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error,
                              "Failed to find " & Source.Display_Base_Name);

                           if not Gnatformat.Command_Line.Keep_Going.Get
                           then
                              exit;
                           end if;

                        else
                           exit when
                           not Process_Standalone_Source
                             (Source,
                              (if Charset.Is_Set
                               then
                                  Ada.Strings.Unbounded.To_String
                                 (Charset.Value)
                               else
                                  Gnatformat.Configuration.Default_Charset))
                              and not Gnatformat
                                .Command_Line
                                  .Keep_Going
                                    .Get;
                        end if;
                     end loop;
                  end;
               end if;

            elsif Base_Commit_ID.Is_Set then
               declare
                  use Gnatformat.Configuration;
                  Charset :
                  constant Optional_Unbounded_String :=
                    Gnatformat.Command_Line.Charset.Get;
               begin
                  Gitdiff.Format_New_Lines
                    (Ada.Strings.Unbounded.To_String (Base_Commit_ID.Value),
                     Gitdiff.Context'
                       (Lal_Ctx          => LAL_Context,
                        Options          =>
                          Gnatformat.Command_Line.Configuration.Get,
                        Unparsing_Config => Unparsing_Configuration,
                        Charset          =>
                          (if Charset.Is_Set
                           then Charset.Value
                           else
                              Ada.Strings.Unbounded.To_Unbounded_String
                             (Default_Charset))));
               end;
            else
               declare
                  use Gnatformat.Project;
                  Command_Line_Sources : constant Project_Source_Vector :=
                    [for Source of Get_Project_Sources (Project_Tree)
                     => Project_Source_Record'
                       (File           =>
                            GNATCOLL.VFS.Create_From_UTF8
                          (Source.Path_Name.String_Value),
                        Visible        => True,
                        Visible_Source => Source)];

               begin
                  for Source of Command_Line_Sources loop
                     exit when
                     not Process_Project_Source (Source)
                       and not Gnatformat.Command_Line.Keep_Going.Get;
                  end loop;
               end;
            end if;

            if Gnatformat.Project.General_Failed then
               GNAT.OS_Lib.OS_Exit (1);
            end if;
         end;

      end;
   end if;

end Gnatformat.Ada_Driver;
