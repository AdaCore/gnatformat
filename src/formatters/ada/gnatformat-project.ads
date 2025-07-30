--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package with the public API for editing sources

with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.VFS;

with Gnatformat.Command_Line;

with GPR2;
with GPR2.Build.Source;      use GPR2.Build.Source;
with GPR2.Build.Source.Sets; use GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;

package Gnatformat.Project is

   GPR_Options : GPR2.Options.Object;

   package Source_Lists is new
     Ada.Containers.Doubly_Linked_Lists
       (Element_Type => GPR2.Build.Source.Object);

   subtype Source_List is Source_Lists.List;

   type Project_Source_Record (Visible : Boolean := True) is record
      File : GNATCOLL.VFS.Virtual_File;

      case Visible is
         when True =>
            Visible_Source : GPR2.Build.Source.Object;

         when False =>
            null;
      end case;
   end record;

   No_Project_Source : constant Project_Source_Record :=
     (Visible => False, File => GNATCOLL.VFS.No_File);

   package Project_Source_Vectors is new
     Ada.Containers.Vectors (Positive, Project_Source_Record);

   subtype Project_Source_Vector is Project_Source_Vectors.Vector;

   function To_Project_Source
     (Project_Tree : GPR2.Project.Tree.Object;
      Source       : GNATCOLL.VFS.Virtual_File) return Project_Source_Record;
   --  Transforms the GNATCOLL.VFS.Virtual_File into a Project_Source_Record
   --  in the context of Project_Tree.
   --  Returns No_Project_Source if Source is undefined or externally built.

   function To_Project_Sources
     (Project_Tree : GPR2.Project.Tree.Object;
      Sources      : Gnatformat.Command_Line.Sources.Result_Array)
      return Project_Source_Vector;
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

   procedure Set_General_Failed;
   function General_Failed return Boolean;

private

   General_Failed_Flag : Boolean := False;

end Gnatformat.Project;
