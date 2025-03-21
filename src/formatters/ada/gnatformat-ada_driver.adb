--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
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
with Gnatformat.Edits;
with Gnatformat.Formatting;
with Gnatformat.Helpers;

with GPR2;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;

with Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers;
with Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Preprocessing;

with Gitdiff;

procedure Gnatformat.Ada_Driver is

   GPR_Options : GPR2.Options.Object;

   General_Failed : Boolean := False;

   package Langkit_Support_Unparsing
     renames Langkit_Support.Generic_API.Unparsing;

   package Source_Lists is new
     Ada.Containers.Doubly_Linked_Lists
       (Element_Type => GPR2.Build.Source.Object);

   subtype Source_List is Source_Lists.List;

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

   type Project_Source_Record (Visible : Boolean := True) is record
      File : GNATCOLL.VFS.Virtual_File;

      case Visible is
         when True =>
            Visible_Source : GPR2.Build.Source.Object;

         when False =>
            null;
      end case;
   end record;

   package Project_Source_Vectors is new
     Ada.Containers.Vectors (Positive, Project_Source_Record);

   subtype Project_Source_Vector is Project_Source_Vectors.Vector;

   function Get_Command_Line_Sources
     (Project_Tree : GPR2.Project.Tree.Object) return Project_Source_Vector;
   --  Transforms the Gnatformat.Command_Line.Sources provided by the user into
   --  an Command_Line_Source_Vector.

   function Get_Project_Sources
     (Project_Tree : GPR2.Project.Tree.Object) return Source_List;
   --  Transforms the Gnatformat.Command_Line.Sources provided by the user into
   --  an GPR2.Project.Source.Set.Object.

   procedure Load_Project
     (Project_Tree : in out GPR2.Project.Tree.Object;
      Project_File : GNATCOLL.VFS.Virtual_File);
   --  Elabores the GPR2 registry and loads the project given by Project_File

   function Resolve_Project_File return GNATCOLL.VFS.Virtual_File;
   --  Finds the correct project file.
   --  First tries to retrieve it from the -P command line options.
   --  If not provided, tries to find an implicit project in the current
   --  working directory. If more than one project found, then none are
   --  returned.

   ------------------------------
   -- Get_Command_Line_Sources --
   ------------------------------

   function Get_Command_Line_Sources
     (Project_Tree : GPR2.Project.Tree.Object) return Project_Source_Vector
   is
      Sources : Project_Source_Vector;

   begin
      Gnatformat_Trace.Trace ("Getting command line sources");

      Project_Tree.Update_Sources;

      for Source of Gnatformat.Command_Line.Sources.Get loop
         Gnatformat_Trace.Trace ("Resolving " & Source.Display_Full_Name);

         declare
            Source_Simple_Name : constant GPR2.Filename_Type :=
              GPR2.Simple_Name (Source.Base_Name);
            Resolved_Source    : GPR2.Build.Source.Object :=
              GPR2.Build.Source.Undefined;

         begin
            --  If Project_Tree is an aggregate project
            --  Project_Tree.Root_Project.Visible_Source raises an exception.
            --  Project_Tree.Root_Project.Sources returns an empty iterable.
            --  Therefore, iterate through each project looking for the source.

            for View of Project_Tree.Namespace_Root_Projects loop
               Resolved_Source := View.Visible_Source (Source_Simple_Name);

               --  FIXME:
               --  Here we use the simple name and ignore the path.
               --  This means that if the user tries to format a source
               --  "foo/a.adb" which does not exist, but a source
               --  "bar/a.adb" exists, then "bar/a.adb" is formatted.
               --  To fix the issue when base name != full name, not only check
               --  if Resolved_Source != Undefined, but also check that if the
               --  relative path of Resolved_Source to the cwd is the same as
               --  base name.

               exit when Resolved_Source /= GPR2.Build.Source.Undefined;
            end loop;

            --  This is an invisible source to the project
            --  Only format if it exists on disk.

            if Resolved_Source = GPR2.Build.Source.Undefined then

               if Source.Is_Regular_File then
                  Sources.Append
                    (Project_Source_Record'(File => Source, Visible => False));

               else
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "Failed to find " & Source.Display_Base_Name);

                  if not Gnatformat.Command_Line.Keep_Going.Get then
                     GNAT.OS_Lib.OS_Exit (1);
                  end if;

                  General_Failed := True;
               end if;

            --  This is an visible source to the project but externally
            --  built. Do not format.

            elsif Resolved_Source.Owning_View.Is_Externally_Built then

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Source.Display_Base_Name & " is an externally built source");

               if not Gnatformat.Command_Line.Keep_Going.Get then
                  GNAT.OS_Lib.OS_Exit (1);
               end if;

               General_Failed := True;

            --  This is an visible source to the project

            else
               Sources.Append
                 (Project_Source_Record'
                    (File           =>
                       GNATCOLL.VFS.Create_From_UTF8
                         (Resolved_Source.Path_Name.String_Value),
                     Visible        => True,
                     Visible_Source => Resolved_Source));
            end if;
         end;
      end loop;

      return Sources;
   end Get_Command_Line_Sources;

   -------------------------
   -- Get_Project_Sources --
   -------------------------

   function Get_Project_Sources
     (Project_Tree : GPR2.Project.Tree.Object) return Source_List
   is
      Sources : Source_List;

      procedure Process_Part
        (Kind     : GPR2.Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : GPR2.Path_Name.Object;
         Index    : GPR2.Unit_Index;
         Sep_Name : GPR2.Optional_Name_Type);
      --  Create a source from View and Path and add to Sources.
      --  If the source cannot be resolved, then exit with error code 1 if the
      --  `--keep-going` switch was not provided. Otherwise, set General_Failed
      --  to True.

      procedure Process_Unit (Unit : GPR2.Build.Compilation_Unit.Object);
      --  Call Process_Part for all parts of this unit

      ------------------
      -- Process_Part --
      ------------------

      procedure Process_Part
        (Kind     : GPR2.Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : GPR2.Path_Name.Object;
         Index    : GPR2.Unit_Index;
         Sep_Name : GPR2.Optional_Name_Type)
      is
         pragma Unreferenced (Kind, Index, Sep_Name);

         Resolved_Source : constant GPR2.Build.Source.Object :=
           View.Source (Path.Simple_Name);

      begin
         if Resolved_Source = GPR2.Build.Source.Undefined then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Failed to resolve " & String (Path.Simple_Name));

            if not Gnatformat.Command_Line.Keep_Going.Get then
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            General_Failed := True;

         else
            Sources.Append (Resolved_Source);
         end if;
      end Process_Part;

      ------------------
      -- Process_Unit --
      ------------------

      procedure Process_Unit (Unit : GPR2.Build.Compilation_Unit.Object) is
      begin
         Unit.For_All_Part (Process_Part'Access);
      end Process_Unit;

   begin
      Project_Tree.For_Each_Ada_Closure
        (Action            => Process_Unit'Access,
         All_Sources       => Gnatformat.Command_Line.All_Sources.Get,
         Root_Project_Only => Gnatformat.Command_Line.Root_Project_Only.Get,
         Externally_Built  => False);

      return Sources;
   end Get_Project_Sources;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project_Tree : in out GPR2.Project.Tree.Object;
      Project_File : GNATCOLL.VFS.Virtual_File)
   is
      use GPR2.Reporter;
      Reporter : Console.Object := Console.Create;

   begin
      Console.Set_User_Verbosity (Reporter, Important_Only);

      if not Project_Tree.Load
               (Options          => GPR_Options,
                Reporter         => Reporter,
                Absent_Dir_Error => GPR2.No_Error,
                With_Runtime     => True)
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Failed to load project """
            & Project_File.Display_Full_Name (Normalize => True)
            & """");
         GNAT.OS_Lib.OS_Exit (1);

      else
         Gnatformat_Trace.Trace
           ("Successfully loaded project """
            & Project_File.Display_Full_Name (Normalize => True)
            & """");
      end if;
      Project_Tree.Update_Sources;
   end Load_Project;

   --------------------------
   -- Resolve_Project_File --
   --------------------------

   function Resolve_Project_File return GNATCOLL.VFS.Virtual_File is
      function Find_Implicit_Project_File return GNATCOLL.VFS.Virtual_File;
      --  Searches in the current directory for an unique .gpr file.
      --  If multiple exist, returns No_File.

      ---------------------------
      -- Find_Implicit_Project --
      ---------------------------

      function Find_Implicit_Project_File return GNATCOLL.VFS.Virtual_File is
         Search        : Ada.Directories.Search_Type;
         Current_Entry : Ada.Directories.Directory_Entry_Type;

         Result : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;

      begin
         Gnatformat_Trace.Trace
           ("Trying to find implicit projects in """
            & Ada.Directories.Current_Directory
            & """");

         Ada.Directories.Start_Search
           (Search    => Search,
            Directory => Ada.Directories.Current_Directory,
            Pattern   => "",
            Filter    =>
              [Ada.Directories.Ordinary_File => True, others => False]);

         while Ada.Directories.More_Entries (Search) loop
            Ada.Directories.Get_Next_Entry (Search, Current_Entry);

            if Ada.Directories.Extension
                 (Ada.Directories.Simple_Name (Current_Entry))
              = "gpr"
            then
               if Result = GNATCOLL.VFS.No_File then
                  Gnatformat_Trace.Trace
                    ("Found implicit project """
                     & Ada.Directories.Full_Name (Current_Entry)
                     & """");

                  Result :=
                    GNATCOLL.VFS.Create
                      (GNATCOLL.VFS."+"
                         (Ada.Directories.Full_Name (Current_Entry)));

               else
                  Gnatformat_Trace.Trace
                    ("Found another implicit project """
                     & Ada.Directories.Full_Name (Current_Entry)
                     & """ - only one is allowed");

                  return GNATCOLL.VFS.No_File;
               end if;
            end if;
         end loop;

         Ada.Directories.End_Search (Search);

         if Result = GNATCOLL.VFS.No_File then
            Gnatformat_Trace.Trace
              ("No implicit project was found in """
               & Ada.Directories.Current_Directory
               & """");
         end if;

         return Result;
      end Find_Implicit_Project_File;

      Explicit_Project : GNATCOLL.VFS.Virtual_File :=
        (if GPR_Options.Project_File.Is_Defined
         then
           GNATCOLL.VFS.Create_From_UTF8
             (String (GPR_Options.Project_File.Name))
         else No_File);

   begin
      Gnatformat_Trace.Trace ("Resolving project file");

      if Explicit_Project = GNATCOLL.VFS.No_File
        or Explicit_Project.Display_Full_Name = ""
      then
         Gnatformat_Trace.Trace ("No explicit project was provided");

         return Find_Implicit_Project_File;

      else
         if GNATCOLL.VFS."+" (Explicit_Project.File_Extension) /= ".gpr" then
            Gnatformat_Trace.Trace
              ("Explicitly provided project does not have a .gpr extension "
               & "- implicitly adding it");

            return
              GNATCOLL.VFS.Create_From_UTF8
                (Explicit_Project.Display_Full_Name & ".gpr");

         else
            return Explicit_Project;
         end if;
      end if;
   end Resolve_Project_File;

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
               (Arguments => Unparsed_Arguments, Options => GPR_Options)
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

   GPR_Options.Print_GPR_Registry;

   declare
      procedure Proceed_With_Range_Formatting;
      --  If range format mode is activated, process the selection and
      --  format it.
      --  If --pipe is used, then prints the formatted source to stdout.

      -------------------------------------
      --  Proceed_With_Range_Formatting  --
      -------------------------------------

      procedure Proceed_With_Range_Formatting
      is
         use type Gnatformat.Command_Line.Sources.Result_Array;
         use type Langkit_Support_Unparsing.Unparsing_Configuration;
         use Langkit_Support.Slocs;

         Project_File : constant GNATCOLL.VFS.Virtual_File :=
           Resolve_Project_File;
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
         if Unparsing_Configuration =
           Langkit_Support_Unparsing.No_Unparsing_Configuration
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

         --  Load the project file
         if Project_File /= GNATCOLL.VFS.No_File then
            Load_Project (Project_Tree, Project_File);
         end if;

         if Project_Tree.Is_Defined then
            declare
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
   begin

      --  Gnatformat range formatting mode is activated
      if Gnatformat.Command_Line.Range_Format.Get then
         Proceed_With_Range_Formatting;
      else
         --  GNATformat normal driver behavior
         declare
            use type Gnatformat.Command_Line.Sources.Result_Array;
            use type Langkit_Support_Unparsing.Unparsing_Configuration;
            use Langkit_Support.Diagnostics;

            Project_File : constant GNATCOLL.VFS.Virtual_File :=
              Resolve_Project_File;
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
               Load_Project (Project_Tree, Project_File);
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
                 (Source         : Project_Source_Record;
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
                 (Source         : Project_Source_Record;
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
                              General_Failed := True;

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
                                    General_Failed := True;
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
                     General_Failed := True;

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
                        General_Failed := True;

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
                                    General_Failed := True;
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
                     General_Failed := True;

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
                  General_Failed := True;

               elsif Gnatformat.Command_Line.Sources.Get'Length > 0 then
                  --  Only print the source simple name as a comment if there's
                  --- more than one source to format.

                  Print_Source_Simple_Name :=
                    Gnatformat.Command_Line.Sources.Get'Length > 1;

                  if Project_Tree.Is_Defined then
                     declare
                        Command_Line_Sources :
                        constant Project_Source_Vector :=
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
                              General_Failed := True;

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

               if General_Failed then
                  GNAT.OS_Lib.OS_Exit (1);
               end if;
            end;

         end;
      end if;
   end;

end Gnatformat.Ada_Driver;
