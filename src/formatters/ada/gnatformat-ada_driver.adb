--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNAT.OS_Lib;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gnatformat.Command_Line;
with Gnatformat.Command_Line.Configuration;
with Gnatformat.Configuration;
with Gnatformat.Formatting;

with GPR2;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;
with Libadalang.Generic_API;

procedure Gnatformat.Ada_Driver
is
   package Langkit_Support_Unparsing
     renames Langkit_Support.Generic_API.Unparsing;

   package Source_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => GPR2.Build.Source.Object);

   function Get_Command_Line_Sources
     (Project_Tree : GPR2.Project.Tree.Object; Failed : out Boolean)
      return Source_List.List;
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
     (Project_Tree : GPR2.Project.Tree.Object; Failed : out Boolean)
      return Source_List.List
   is
      Sources : Source_List.List;

      Root_Project : constant GPR2.Project.View.Object :=
        Project_Tree.Root_Project;
   begin
      Gnatformat_Trace.Trace ("Getting command line sources");

      Failed := False;

      Project_Tree.Update_Sources;

      for Source of Gnatformat.Command_Line.Sources.Get loop
         Gnatformat_Trace.Trace ("Resolving " & Source.Display_Full_Name);
         declare
            Resolved_Source : constant GPR2.Build.Source.Object :=
              Root_Project.Source (GPR2.Simple_Name (Source.Base_Name));

            use type GPR2.Build.Source.Object;

         begin
            if Resolved_Source = GPR2.Build.Source.Undefined then
               Failed := True;

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Failed to resolve " & Source.Display_Base_Name);
               if not Gnatformat.Command_Line.Keep_Going.Get then
                  GNAT.OS_Lib.OS_Exit (1);
               end if;

            else
               Sources.Append (Resolved_Source);
            end if;
         end;
      end loop;

      return Sources;
   end Get_Command_Line_Sources;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project_Tree : in out GPR2.Project.Tree.Object;
      Project_File : GNATCOLL.VFS.Virtual_File)
   is
      Options : GPR2.Options.Object;

   begin
      Gnatformat.Configuration.Elaborate_GPR2;

      if Project_File /= GNATCOLL.VFS.No_File then
         Project_File.Normalize_Path;

         Options.Add_Switch
           (GPR2.Options.P, GNATCOLL.VFS."+" (Project_File.Full_Name));
      end if;

      for Scenario_Variable of Gnatformat.Command_Line.Scenario.Get loop
         Options.Add_Switch
           (GPR2.Options.X,
            Ada.Strings.Unbounded.To_String (Scenario_Variable));
      end loop;

      --  Do not emit project load warnings
      GPR2.Project.Tree.Verbosity := GPR2.Project.Tree.Quiet;

      if not Project_Tree.Load
        (Options,
         Absent_Dir_Error => GPR2.No_Error)
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Failed to load project """
            & Project_File.Display_Full_Name (Normalize => True)
            & """");
         Project_Tree.Log_Messages.Output_Messages;
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

      Unparsing_Configuration_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Unparsing_Configuration.Get;
      Unparsing_Configuration :
        constant Langkit_Support_Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Load_Unparsing_Configuration
          (Unparsing_Configuration_File, Diagnostics);

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

         Project_Format_Options_Cache :
           Gnatformat.Configuration.Project_Format_Options_Cache_Type :=
             Gnatformat
               .Configuration
               .Create_Project_Format_Options_Cache;

         function Format_Source
           (Path : GPR2.Path_Name.Object;
            View : GPR2.Project.View.Object)
            return Ada.Strings.Unbounded.Unbounded_String;
         --  Resolves the right format options for Path and formats it.
         --  View is the owning view of the source.

         -------------------
         -- Format_Source --
         -------------------

         function Format_Source
           (Path : GPR2.Path_Name.Object;
            View : GPR2.Project.View.Object)
            return Ada.Strings.Unbounded.Unbounded_String
         is
            Unit : constant Libadalang.Analysis.Analysis_Unit :=
              LAL_Context.Get_From_File (String (Path.Value));
            Project_Formatting_Config :
              Gnatformat.Configuration.Format_Options_Type :=
                Project_Format_Options_Cache.Get (View);

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

         General_Failed : Boolean := False;

         procedure Process_Part
           (Kind     : GPR2.Unit_Kind;
            View     : GPR2.Project.View.Object;
            Path     : GPR2.Path_Name.Object;
            Index    : GPR2.Unit_Index;
            Sep_Name : GPR2.Optional_Name_Type);
         --  Process one Part (ie, one source)

         procedure Process_Source
           (Path : GPR2.Path_Name.Object;
            View : GPR2.Project.View.Object);

         procedure Process_Unit (Unit : GPR2.Build.Compilation_Unit.Object);
         --  Process one compilation unit

         procedure Process_Unit (Unit : GPR2.Build.Compilation_Unit.Object) is
         begin
            Unit.For_All_Part (Process_Part'Access);
         end Process_Unit;

         procedure Process_Source
           (Path : GPR2.Path_Name.Object;
            View : GPR2.Project.View.Object) is
         begin
            if Gnatformat.Command_Line.Pipe.Get then
               begin
                  --  TODO: do we need to output the file name?
                  Ada.Text_IO.Put_Line
                    (String (Path.Base_Filename) & String (Path.Extension));
                  Ada.Strings.Unbounded.Text_IO.Put_Line
                    (Format_Source (Path, View));
                  Ada.Text_IO.New_Line;

               exception
                  when E : others =>
                     General_Failed := True;

                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Failed to format "
                        & String (Path.Value));

                     if Gnatformat.Command_Line.Keep_Going.Get then
                        Gnatformat_Trace.Trace
                          (Ada.Exceptions.Exception_Information (E));

                     else
                        Ada.Exceptions.Reraise_Occurrence (E);
                     end if;
               end;
            else
               declare
                  Formatted_Source :
                    constant Ada.Strings.Unbounded.Unbounded_String :=
                      Format_Source (Path, View);
                  Source_File      : Ada.Text_IO.File_Type;

               begin
                  Ada.Text_IO.Create
                    (File => Source_File,
                     Mode => Ada.Text_IO.Out_File,
                     Name => String (Path.Value));

                  Ada.Text_IO.Unbounded_IO.Put (Source_File, Formatted_Source);

                  Ada.Text_IO.Close (Source_File);

               exception
                  when E : others =>
                     General_Failed := True;

                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Failed to format " & String (Path.Value));

                     if Gnatformat.Command_Line.Keep_Going.Get then
                        Gnatformat_Trace.Trace
                          (Ada.Exceptions.Exception_Information (E));

                     else
                        Ada.Exceptions.Reraise_Occurrence (E);
                     end if;
               end;
            end if;
         end Process_Source;

         procedure Process_Part
           (Kind     : GPR2.Unit_Kind;
            View     : GPR2.Project.View.Object;
            Path     : GPR2.Path_Name.Object;
            Index    : GPR2.Unit_Index;
            Sep_Name : GPR2.Optional_Name_Type)
         is
            pragma Unreferenced (Kind, Index, Sep_Name);
         begin
            Process_Source (Path, View);
         end Process_Part;

         use Source_List;
         Command_Line_Sources : constant Source_List.List :=
           Get_Command_Line_Sources (Project_Tree, General_Failed);
      begin
         if not Command_Line_Sources.Is_Empty then
            for Source of Command_Line_Sources loop
               Process_Source (Source.Path_Name, Source.Owning_View);
            end loop;
         else
            Project_Tree.For_Each_Ada_Closure
              (Process_Unit'Access,
                All_Sources       =>
                  Gnatformat.Command_Line.Process_All_Files.Get,
                Root_Project_Only =>
                  Gnatformat.Command_Line.No_Subprojects.Get,
                Externally_Built  => False);
         end if;

         if General_Failed then
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;
   end;
end Gnatformat.Ada_Driver;
