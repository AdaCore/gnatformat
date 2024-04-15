--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

with Gnatformat.Command_Line;

with GPR2;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.Source;
with GPR2.Project.Source.Set;
with GPR2.Project.Source.Part_Set;
with GPR2.View_Ids;
with GPR2.View_Ids.Set;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;
with Libadalang.Generic_API;

with Prettier_Ada.Documents;

procedure Gnatformat.Ada_Driver
is

   package Langkit_Support_Unparsing
     renames Langkit_Support.Generic_API.Unparsing;

   function Get_Sources
     (Project_Tree      : GPR2.Project.Tree.Object;
      Root_Project_Only : Boolean := False;
      All_Files         : Boolean := False)
      return GPR2.Project.Source.Set.Object;
   --  TODO

   function Load_Formatting_Rules
     (Diagnostics :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support_Unparsing.Unparsing_Configuration;
   --  Loads the formatting rules

   procedure Load_Project (Project_Tree : in out GPR2.Project.Tree.Object);
   --  TODO

   function Parse_Formatting_Config
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Loads the formatting rules

   -----------------
   -- Get_Sources --
   -----------------

   function Get_Sources
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
                  Main_Source : constant GPR2.Project.Source.Object :=
                    Project_View.Source (Main.Source);

               begin
                  Gnatformat_Trace.Trace
                    ("Getting main file """
                     & Main_Source.Path_Name.Value
                     & """ dependencies");

                  Sources.Include (Main_Source);

                  for Source_Dependency
                    of Main_Source.Dependencies
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
   end Get_Sources;

   -----------------------------
   --  Load_Unparsing_Config  --
   -----------------------------

   function Load_Formatting_Rules
     (Diagnostics :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support_Unparsing.Unparsing_Configuration
   is
      use type GNATCOLL.VFS.Virtual_File;

      Rules_File : constant GNATCOLL.VFS.Virtual_File :=
        Gnatformat.Command_Line.Rules.Get;

   begin
      if Rules_File = GNATCOLL.VFS.No_File then
         raise Program_Error
           with "Formatting rules (unparsing configuration) must be provided";
      end if;

      declare
         Rules_File_Name : constant String :=
           GNATCOLL.VFS."+" (Rules_File.Full_Name);

      begin
         Gnatformat.Gnatformat_Trace.Trace
           ("Loading formatting rules from """ & Rules_File_Name & """");

         return
           Langkit_Support_Unparsing.Load_Unparsing_Config
             (Libadalang.Generic_API.Ada_Lang_Id,
              Rules_File_Name,
              Diagnostics);
      end;
   end Load_Formatting_Rules;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project (Project_Tree : in out GPR2.Project.Tree.Object)
   is
      use type GNATCOLL.VFS.Virtual_File;

      function Resolve_Project_File return GNATCOLL.VFS.Virtual_File;
      --  TODO

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
         begin
            raise Program_Error with "TODO";
            return GNATCOLL.VFS.No_File;
         end Find_Implicit_Project_File;

      begin
         return
           (declare
              Explicit_Project : constant GNATCOLL.VFS.Virtual_File :=
                Gnatformat.Command_Line.Project.Get;
            begin
              (if Explicit_Project = GNATCOLL.VFS.No_File
               then Find_Implicit_Project_File
               else Explicit_Project));
      end Resolve_Project_File;

      Project_File : constant GNATCOLL.VFS.Virtual_File :=
        Resolve_Project_File;

      Options : GPR2.Options.Object;

   begin
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
            GNAT.OS_Lib.OS_Exit (1);

         else
            Gnatformat_Trace.Trace
              ("Successfully loaded project """
               & Project_File.Display_Full_Name (Normalize => True)
               & """");
         end if;
      end if;
   end Load_Project;

   -----------------------------
   -- Parse_Formatting_Config --
   -----------------------------

   function Parse_Formatting_Config
      return Prettier_Ada.Documents.Format_Options_Type
   is
   begin
      --  TODO
      return Prettier_Ada.Documents.Default_Format_Options;
   end Parse_Formatting_Config;

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

      Formatting_Config :
        constant Prettier_Ada.Documents.Format_Options_Type :=
          Parse_Formatting_Config;

      Diagnostics      :
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Formatting_Rules :
        constant Langkit_Support_Unparsing.Unparsing_Configuration :=
          Load_Formatting_Rules (Diagnostics);

   begin
      if Formatting_Rules
         = Langkit_Support_Unparsing.No_Unparsing_Configuration
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "Failed to load formatting rules");
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

      if Gnatformat.Command_Line.Project.Get = GNATCOLL.VFS.No_File
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

      else
         declare
            Project_Tree : GPR2.Project.Tree.Object;

         begin
            Load_Project (Project_Tree);

            declare
               LAL_Context :
                 constant Libadalang.Analysis.Analysis_Context :=
                   Libadalang.Analysis.Create_Context_From_Project
                     (Project_Tree);

               Sources : constant GPR2.Project.Source.Set.Object :=
                 Get_Sources
                   (Project_Tree,
                    Gnatformat.Command_Line.No_Subprojects.Get,
                    Gnatformat.Command_Line.Process_All_Files.Get);

            begin
               for Source of Sources loop
                  declare
                     Unit_Ignore :
                       constant Langkit_Support.Generic_API.Analysis.Lk_Unit :=
                         Libadalang.Generic_API.To_Generic_Unit
                           (LAL_Context.Get_From_File
                              (Source.Path_Name.Value));
                  begin
                     --  TODO: Handle unit
                     null;
                  end;
               end loop;
            end;
         end;
      end if;
   end;
end Gnatformat.Ada_Driver;
