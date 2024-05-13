--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNAT.OS_Lib;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

with Gnatformat.Command_Line;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Configuration;
with Gnatformat.Formatting;

with GPR2;

with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Source;
with GPR2.Project.Source.Part_Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.View_Ids;
with GPR2.View_Ids.Set;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;

procedure Gnatformat.Ada_Driver
is

   package Langkit_Support_Unparsing
   renames Langkit_Support.Generic_API.Unparsing;

   function Get_Command_Line_Sources
     (Project_Tree : GPR2.Project.Tree.Object; Failed : out Boolean)
      return GPR2.Project.Source.Set.Object;
   --  Transforms the Gnatformat.Command_Line.Sources provided by the user into
   --  an GPR2.Project.Source.Set.Object.

   function Get_Project_Sources
     (Project_Tree      : GPR2.Project.Tree.Object;
      Root_Project_Only : Boolean := False;
      All_Files         : Boolean := False)
      return GPR2.Project.Source.Set.Object;
   --  Returns the sources for the Project_Tree
   --  If Root_Project_Only is True, then only the Project_Tree.Root_Project
   --  sources are returned. Otherwise, sources from all views are returned.
   --  If All_Files is True, then all sources of each view are retrieved.
   --  Otherwise, only sources on mains (or library interfaces) closure are
   --  returned.

   function Get_Sources
     (Project_Tree      : GPR2.Project.Tree.Object;
      Root_Project_Only : Boolean := False;
      All_Files         : Boolean := False;
      Failed            : out Boolean)
      return GPR2.Project.Source.Set.Object;
   --  If Gnatformat.Command_Line.Sources contains any sources, meaning that
   --  the user provided a list of sources to be formatted, dispatches to
   --  Get_Command_Line_Sources. Otherwise dispatches to Get_Project_Sources.
   
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
     (Project_Tree : GPR2.Project.Tree.Object; Failed : out Boolean)
      return GPR2.Project.Source.Set.Object
   is
      Sources : GPR2.Project.Source.Set.Object;

      Root_Project : constant GPR2.Project.View.Object :=
        Project_Tree.Root_Project;

   begin
      Gnatformat_Trace.Trace ("Getting command line sources");

      Failed := False;

      Project_Tree.Update_Sources;

      for Source of Gnatformat.Command_Line.Sources.Get loop
         Gnatformat_Trace.Trace ("Resolving " & Source.Display_Full_Name);

         declare
            Source_Path     : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create (Source);
            Resolved_Source : constant GPR2.Project.Source.Object :=
              Root_Project.Source (Source_Path);

            use type GPR2.Project.Source.Object;

         begin
            if Resolved_Source = GPR2.Project.Source.Undefined then
               Failed := True;

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Failed to resolve " & Source.Display_Base_Name);

               if not Gnatformat.Command_Line.Keep_Going.Get then
                  GNAT.OS_Lib.OS_Exit (1);
               end if;

            else
               Sources.Include (Resolved_Source);
            end if;
         end;
      end loop;

      return Sources;
   end Get_Command_Line_Sources;

   -------------------------
   -- Get_Project_Sources --
   -------------------------

   function Get_Project_Sources
     (Project_Tree      : GPR2.Project.Tree.Object;
      Root_Project_Only : Boolean := False;
      All_Files         : Boolean := False)
      return GPR2.Project.Source.Set.Object
   is
      Sources : GPR2.Project.Source.Set.Object;

      procedure Process_Project_View_Mains
        (Project_View : GPR2.Project.View.Object;
         Valid_Views  : GPR2.View_Ids.Set.Set);
      --  Adds main sources (or library interfaces) and its dependencies to
      --  Sources if it belong to a view of Valid_Views. If Project_View does
      --  not have mains and is not a library, then no sources are added.

      --------------------------------
      -- Process_Project_View_Mains --
      --------------------------------

      procedure Process_Project_View_Mains
        (Project_View : GPR2.Project.View.Object;
         Valid_Views  : GPR2.View_Ids.Set.Set)
      is
      begin
         if Project_View.Has_Mains then
            Gnatformat_Trace.Trace ("Project has mains");

            for Main of Project_View.Mains loop
               declare
                  Main_Source              :
                    constant GPR2.Project.Source.Object          :=
                      Project_View.Source (Main.Source);
                  Main_Source_Dependencies :
                    constant GPR2.Project.Source.Part_Set.Object :=
                      Main_Source.Dependencies;

               begin
                  Gnatformat_Trace.Trace
                    ("Getting main file """
                     & Main_Source.Path_Name.Value
                     & """ dependencies");

                  Sources.Include (Main_Source);

                  for Source_Dependency of Main_Source_Dependencies
                    when Valid_Views.Contains
                           (Source_Dependency.Source.View.Id)
                  loop
                     Sources.Include (Source_Dependency.Source);
                  end loop;
               end;
            end loop;

         elsif Project_View.Is_Library then
            Gnatformat_Trace.Trace ("Project is a library");

            if not Project_View.Has_Any_Interfaces then
               Gnatformat_Trace.Trace
                 ("Project does not have any interfaces");

            else
               for Library_Interface
                  of Project_View.Sources (Interface_Only => True)
               loop
                  Gnatformat_Trace.Trace
                    ("Getting library interface """
                     & Library_Interface.Path_Name.Value
                     & """ dependencies:");

                  Sources.Include (Library_Interface);

                  for Source_Dependency
                    of Library_Interface.Dependencies
                    when Valid_Views.Contains
                           (Source_Dependency.Source.View.Id)
                  loop
                     Sources.Include (Source_Dependency.Source);
                  end loop;
               end loop;
            end if;

         else
            Gnatformat_Trace.Trace
              ("Project does not have any mains nor library interfaces. "
               & "Getting all sources");

            Sources.Union (Project_View.Sources);
         end if;
      end Process_Project_View_Mains;

   begin
      Gnatformat_Trace.Trace ("Getting project sources");

      Project_Tree.Update_Sources;

      if Root_Project_Only then
         --  Switch --no-subproject

         declare
            Root_Project : constant GPR2.Project.View.Object :=
              Project_Tree.Root_Project;
            Valid_Views  : constant GPR2.View_Ids.Set.Set :=
              [Root_Project.Id];

         begin
            if All_Files then
               --  No switch -U

               Gnatformat_Trace.Trace
                 ("Getting all root project """
                  & Root_Project.Path_Name.Value
                  & """ sources");

               Sources.Union (Project_Tree.Root_Project.Sources);

            else
               --  Switch -U

               Gnatformat_Trace.Trace
                 ("Getting root project """
                  & Root_Project.Path_Name.Value
                  & """ sources that depend on main or library interface "
                  & "sources");

               Process_Project_View_Mains (Root_Project, Valid_Views);
            end if;
         end;

      else
         --  No switch --no-subproject

         if All_Files then
            --  No switch -U

            Gnatformat_Trace.Trace ("Getting all projects sources");

            for Project_View of Project_Tree
              when not Project_View.Is_Externally_Built
            loop
               Gnatformat_Trace.Trace
                 ("Getting project """
                  & Project_View.Path_Name.Value
                  & """ sources");
               Sources.Union (Project_View.Sources);
            end loop;

         else
            --  Switch -U

            Gnatformat_Trace.Trace
              ("Getting all projects sources that depend on main or library "
               & "interface sources");

            declare
               Project_View_Ids : constant GPR2.View_Ids.Set.Set :=
                 [for Project_View of Project_Tree
                  when not Project_View.Is_Externally_Built
                  => Project_View.Id];

            begin
               for Project_View of Project_Tree
                 when not Project_View.Is_Externally_Built
               loop
                  Gnatformat_Trace.Trace
                    ("Getting project """
                     & Project_View.Path_Name.Value
                     & """ sources");

                  Process_Project_View_Mains (Project_View, Project_View_Ids);
               end loop;
            end;
         end if;
      end if;

      return Sources;
   end Get_Project_Sources;

   -----------------
   -- Get_Sources --
   -----------------

   function Get_Sources
     (Project_Tree      : GPR2.Project.Tree.Object;
      Root_Project_Only : Boolean := False;
      All_Files         : Boolean := False;
      Failed            : out Boolean)
      return GPR2.Project.Source.Set.Object
   is
      use type Gnatformat.Command_Line.Sources.Result_Array;

   begin
      Gnatformat_Trace.Trace ("Getting sources to be formatted");

      if Gnatformat.Command_Line.Sources.Get
          = Gnatformat.Command_Line.Sources.No_Results
      then
         Gnatformat_Trace.Trace
           ("User did not provide a list of sources to be formatted");

         Failed := False;

         return
           Get_Project_Sources (Project_Tree, Root_Project_Only, All_Files);

      else
         Gnatformat_Trace.Trace
           ("User provided a list of sources to be formatted");

         return Get_Command_Line_Sources (Project_Tree, Failed);
      end if;
   end Get_Sources;
   
   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project_Tree : in out GPR2.Project.Tree.Object;
      Project_File : GNATCOLL.VFS.Virtual_File)
   is
      use type GPR2.Message.Level_Value;
      use type GNATCOLL.VFS.Virtual_File;

      Options : GPR2.Options.Object;

   begin
      Gnatformat_Trace.Trace ("Loading project");

      Gnatformat.Configuration.Elaborate_GPR2;

      if Project_File /= GNATCOLL.VFS.No_File then

         Project_File.Normalize_Path;

         Options.Add_Switch
           (GPR2.Options.P, GNATCOLL.VFS."+" (Project_File.Full_Name));
         for Scenario_Variable of Gnatformat.Command_Line.Scenario.Get loop
            Options.Add_Switch
              (GPR2.Options.X,
               Ada.Strings.Unbounded.To_String (Scenario_Variable));
         end loop;
         Options.Finalize (Allow_Implicit_Project => False, Quiet =>  True);

         if not Options.Load_Project
                  (Tree             => Project_Tree,
                   Absent_Dir_Error => GPR2.Project.Tree.No_Error,
                   Quiet            => True)
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Failed to load project """
               & Project_File.Display_Full_Name (Normalize => True)
               & """");
            for Log_Message of Project_Tree.Log_Messages.all
               when Log_Message.Level = GPR2.Message.Error
            loop
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error, Log_Message.Message);
            end loop;
            GNAT.OS_Lib.OS_Exit (1);

         else
            Gnatformat_Trace.Trace
              ("Successfully loaded project """
               & Project_File.Display_Full_Name (Normalize => True)
               & """");
         end if;
      end if;
   end Load_Project;

   --------------------------
   -- Resolve_Project_File --
   --------------------------

   function Resolve_Project_File return GNATCOLL.VFS.Virtual_File is
      use type GNATCOLL.VFS.Virtual_File;

      function Find_Implicit_Project_File return GNATCOLL.VFS.Virtual_File;
      --  Searches in the current directory for an unique .gpr file.
      --  If multiple exist, returns No_File.

      ---------------------------
      -- Find_Implicit_Project --
      ---------------------------

      function Find_Implicit_Project_File return GNATCOLL.VFS.Virtual_File
      is
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

            if Ada.Directories.Extension (Current_Entry.Simple_Name) = "gpr"
            then
               if Result = GNATCOLL.VFS.No_File then
                  Gnatformat_Trace.Trace
                    ("Found implicit project """
                     & Current_Entry.Full_Name
                     & """");

                  Result :=
                    GNATCOLL.VFS.Create
                      (GNATCOLL.VFS."+" (Current_Entry.Full_Name));

               else
                  Gnatformat_Trace.Trace
                    ("Found another implicit project """
                     & Current_Entry.Full_Name
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

      Explicit_Project : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Project.Get;

   begin
      Gnatformat_Trace.Trace ("Resolving project file");

      if Explicit_Project = GNATCOLL.VFS.No_File then
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

   if not Gnatformat.Command_Line.Parser.Parse then
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if Gnatformat.Command_Line.Version.Get then
      Ada.Text_IO.Put_Line
        ("GNATformat " & Gnatformat.Version & " " & Gnatformat.Build_Date);
      return;
   end if;

   if Gnatformat.Command_Line.Verbose.Get then
      Gnatformat_Trace.Set_Active (True);
   end if;

   Gnatformat_Trace.Trace
     ("GNATformat " & Gnatformat.Version & " " & Gnatformat.Build_Date);

   declare
      use type GNATCOLL.VFS.Virtual_File;
      use type Gnatformat.Command_Line.Sources.Result_Array;
      use type Langkit_Support_Unparsing.Unparsing_Configuration;

      Project_File : constant GNATCOLL.VFS.Virtual_File :=
        Resolve_Project_File;
      Project_Tree : GPR2.Project.Tree.Object;

      CLI_Formatting_Config :
        constant Gnatformat.Configuration.Format_Options_Type :=
          Gnatformat.Command_Line.Configuration.Get;

      Diagnostics             :
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;

      Unparsing_Conf_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Unparsing_Configuration.Get;

      Unparsing_Configuration :
        constant Langkit_Support_Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Load_Unparsing_Configuration
          (Unparsing_Conf_File, Diagnostics);

   begin
      if Project_File = GNATCOLL.VFS.No_File
        and Gnatformat.Command_Line.Sources.Get
            = Gnatformat.Command_Line.Sources.No_Results
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Either a project or sources must be provided.");
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            GNATCOLL.Opt_Parse.Help (Gnatformat.Command_Line.Parser));
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

      Load_Project (Project_Tree, Project_File);

      declare
         LAL_Context :
           constant Libadalang.Analysis.Analysis_Context :=
             Libadalang.Analysis.Create_Context_From_Project
               (Project_Tree);

         Get_Sources_Failed : Boolean;
         Sources : constant GPR2.Project.Source.Set.Object :=
           Get_Sources
             (Project_Tree,
              Gnatformat.Command_Line.No_Subprojects.Get,
              Gnatformat.Command_Line.Process_All_Files.Get,
              Get_Sources_Failed);

         Project_Format_Options_Cache :
           Gnatformat.Configuration.Project_Format_Options_Cache_Type :=
             Gnatformat
               .Configuration
               .Create_Project_Format_Options_Cache;

         function Format_Source
           (Source : GPR2.Project.Source.Object)
            return Ada.Strings.Unbounded.Unbounded_String;
         --  Resolves the right format options for Source and formats it

         -------------------
         -- Format_Source --
         -------------------

         function Format_Source
           (Source : GPR2.Project.Source.Object)
            return Ada.Strings.Unbounded.Unbounded_String
         is
            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              LAL_Context.Get_From_File (Source.Path_Name.Value);
            Project_Formatting_Config :
              Gnatformat.Configuration.Format_Options_Type :=
                Project_Format_Options_Cache.Get (Source.View);

         begin
            --  TODO: Cache the override
            Project_Formatting_Config.Overwrite
              (CLI_Formatting_Config);

            return
              Gnatformat.Formatting.Format
                (Unit           => Unit,
                 Format_Options => Project_Formatting_Config,
                 Configuration  => Unparsing_Configuration);
         end Format_Source;

         General_Failed : Boolean := Get_Sources_Failed;

      begin
         if Gnatformat.Command_Line.Pipe.Get then
            for Source of Sources loop
               begin
                  Ada.Text_IO.Put_Line
                    (GPR2.Path_Name.Simple_Name (Source.Path_Name.Value));
                  Ada.Strings.Unbounded.Text_IO.Put_Line
                    (Format_Source (Source));
                  Ada.Text_IO.New_Line;

               exception
                  when E : others =>
                     General_Failed := True;

                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Failed to format " &
                          GPR2.Path_Name.Simple_Name (Source.Path_Name.Value));

                     if Gnatformat.Command_Line.Keep_Going.Get then
                        Gnatformat_Trace.Trace
                          (Ada.Exceptions.Exception_Information (E));

                     else
                        Ada.Exceptions.Reraise_Occurrence (E);
                     end if;
               end;
            end loop;

         else
            for Source of Sources loop
               declare
                  Formatted_Source :
                    constant Ada.Strings.Unbounded.Unbounded_String :=
                      Format_Source (Source);
                  Source_File      : Ada.Text_IO.File_Type;

               begin
                  Ada.Text_IO.Create
                    (File => Source_File,
                     Mode => Ada.Text_IO.Out_File,
                     Name => Source.Path_Name.Value);

                  Ada.Text_IO.Unbounded_IO.Put (Source_File, Formatted_Source);

                  Ada.Text_IO.Close (Source_File);

               exception
                  when E : others =>
                     General_Failed := True;

                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Failed to format "
                        & GPR2.Path_Name.Simple_Name (Source.Path_Name.Value));

                     if Gnatformat.Command_Line.Keep_Going.Get then
                        Gnatformat_Trace.Trace
                          (Ada.Exceptions.Exception_Information (E));

                     else
                        Ada.Exceptions.Reraise_Occurrence (E);
                     end if;
               end;
            end loop;
         end if;

         if General_Failed then
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;
   end;
end Gnatformat.Ada_Driver;
