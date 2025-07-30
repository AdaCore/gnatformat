--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Ada.Directories;

with GNAT.OS_Lib;

with GPR2.Build.Compilation_Unit;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Reporter.Console;

package body Gnatformat.Project is

   use GNATCOLL.VFS;

   --------------------------
   --  Set_General_Failed  --
   --------------------------

   procedure Set_General_Failed is
   begin
      General_Failed_Flag := True;
   end Set_General_Failed;

   ----------------------
   --  General_Failed  --
   ----------------------

   function General_Failed return Boolean is
   begin
      return General_Failed_Flag;
   end General_Failed;

   -----------------------
   -- To_Project_Source --
   -----------------------

   function To_Project_Source
     (Project_Tree : GPR2.Project.Tree.Object;
      Source       : GNATCOLL.VFS.Virtual_File) return Project_Source_Record is
   begin
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
               return Project_Source_Record'(File => Source, Visible => False);

            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Failed to find " & Source.Display_Base_Name);

               return No_Project_Source;
            end if;

         --  This is an visible source to the project but externally
         --  built. Do not format.

         elsif Resolved_Source.Owning_View.Is_Externally_Built then

            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Source.Display_Base_Name & " is an externally built source");

            return No_Project_Source;

         --  This is an visible source to the project

         else
            return
              Project_Source_Record'
                (File           =>
                   GNATCOLL.VFS.Create_From_UTF8
                     (Resolved_Source.Path_Name.String_Value),
                 Visible        => True,
                 Visible_Source => Resolved_Source);
         end if;
      end;
   end To_Project_Source;

   ------------------------
   -- To_Project_Sources --
   ------------------------

   function To_Project_Sources
     (Project_Tree : GPR2.Project.Tree.Object;
      Sources      : Gnatformat.Command_Line.Sources.Result_Array)
      return Project_Source_Vector
   is
      Project_Sources : Project_Source_Vector;

   begin
      Gnatformat_Trace.Trace ("Getting command line sources");

      Project_Tree.Update_Sources;

      for Source of Sources loop
         declare
            Project_Source : constant Project_Source_Record :=
              To_Project_Source (Project_Tree, Source);

         begin
            if Project_Source = No_Project_Source then
               if not Gnatformat.Command_Line.Keep_Going.Get then
                  GNAT.OS_Lib.OS_Exit (1);
               end if;
               Set_General_Failed;

            else
               Project_Sources.Append (Project_Source);
            end if;
         end;
      end loop;

      return Project_Sources;
   end To_Project_Sources;

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

            Set_General_Failed;

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

end Gnatformat.Project;
