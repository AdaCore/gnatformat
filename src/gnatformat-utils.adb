--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed;

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with Langkit_Support.Generic_API.Analysis;
with Libadalang.Project_Provider;

package body Gnatformat.Utils is
   use Langkit_Support.Generic_API.Analysis;

   ------------------
   -- Log_Progress --
   ------------------

   procedure Log_Progress
     (Current  : Natural;
      Total    : String;
      Message  : String)
   is
      Total_Length       : constant Natural := Total'Length;
      Current_Image      : constant String :=
        Ada.Strings.Fixed.Tail
          (Ada.Strings.Fixed.Trim (Current'Image, Ada.Strings.Both),
           Total_Length,
           '0');
   begin
      Gnatformat_Trace.Trace
        ("[" & Current_Image & "/" & Total & "] " & Message);
   end Log_Progress;

   procedure Dump_Diagnostics_And_Exit (Unit : Lk_Unit);
   --  Helper procedure to dump diagnostics of the Unit and set the exit status
   --  to failure. Should be called only id the Unit has diagnostics.

   ---------------------------------
   --  Dump_Diagnostics_And_Exit  --
   ---------------------------------

   procedure Dump_Diagnostics_And_Exit (Unit : Lk_Unit)
   is
      pragma Assert (Unit.Has_Diagnostics);
   begin
      Gnatformat_Trace.Trace
        ("WARNING: Source " & Unit.Filename & " has diagnostics"
         & "Cannot parse source file: aborting...");
      for D of Unit.Diagnostics loop
         Gnatformat_Trace.Trace (Unit.Format_GNU_Diagnostic (D));
      end loop;
      Set_Exit_Status (Failure);
   end Dump_Diagnostics_And_Exit;

   ---------------------------------------------------
   --  Get_Analysis_Units_Vector_From_Sources_List  --
   ---------------------------------------------------

   function Get_Analysis_Units_Vector_From_Sources_List
     (Sources          : Sources_List;
      Project_Filename : String := "")
      return Gnatformat.Analysis_Unit_Vectors.Vector
   is
      Units : Vector := Empty_Vector;
   begin
      if Sources'Length = 0 then
         return [];
      end if;

      if Project_Filename /= "" then
         declare
            use GNATCOLL.Projects;
            use GNATCOLL.VFS;

            Project_Environment  : Project_Environment_Access;
            Project_Tree         : constant Project_Tree_Access :=
              new GNATCOLL.Projects.Project_Tree;
            Project_Virtual_File : constant Virtual_File :=
              Create (+Project_Filename);
         begin
            Initialize (Project_Environment);
            Project_Tree.Load
              (Root_Project_Path => Project_Virtual_File,
               Env               => Project_Environment);

            for J of Sources loop
               declare
                  Context : constant Lk_Context := Create_Context (Language);
                  Unit : constant Lk_Unit :=
                    Context.Get_From_File
                      (Filename => Ada.Strings.Unbounded.To_String (J));
               begin
                  if Unit.Has_Diagnostics then
                     Dump_Diagnostics_And_Exit (Unit);
                     return Empty_Vector;
                  else
                     Units.Append (Unit);
                  end if;
               end;
            end loop;
         end;
      else
         for J of Sources loop
            declare
               Context : constant Lk_Context := Create_Context (Language);
               Unit    : constant Lk_Unit :=
                 Context.Get_From_File
                   (Ada.Strings.Unbounded.To_String (J));
            begin
               if Unit.Has_Diagnostics then
                  Dump_Diagnostics_And_Exit (Unit);
                  return Empty_Vector;
               else
                  Units.Append (Unit);
               end if;
            end;
         end loop;
      end if;

      Gnatformat_Trace.Trace ("Found" & Length (Units)'Image & " units");

      return Units;

   end Get_Analysis_Units_Vector_From_Sources_List;

   -----------------------------------------
   --  Get_Project_Analysis_Units_Vector  --
   -----------------------------------------

   function Get_Project_Analysis_Units_Vector
     (Project_Filename : String)
      return Gnatformat.Analysis_Unit_Vectors.Vector
   is
      use GNATCOLL.Projects;
      use GNATCOLL.VFS;
      use Libadalang.Project_Provider;

      Project_Environment  : Project_Environment_Access;
      Project_Tree         : constant Project_Tree_Access :=
        new GNATCOLL.Projects.Project_Tree;
      Project_Virtual_File : constant Virtual_File :=
        Create (+Project_Filename);

      Sources              : Filename_Vectors.Vector;
      Units                : Vector := Empty_Vector;

   begin
      Initialize (Project_Environment);
      Project_Tree.Load
        (Root_Project_Path => Project_Virtual_File,
         Env               => Project_Environment);
      Sources := Source_Files (Project_Tree.all);

      --  Create and append the corresponding Lk_Unit for each source file
      for J of Sources loop
         declare
            Context : constant Lk_Context := Create_Context (Language);
            Unit    : constant Lk_Unit :=
              Context.Get_From_File
                (Filename => Ada.Strings.Unbounded.To_String (J));
         begin
            if Unit.Has_Diagnostics then
               Dump_Diagnostics_And_Exit (Unit);
               return Empty_Vector;
            else
               Units.Append (Unit);
            end if;
         end;
      end loop;

      Gnatformat_Trace.Trace ("Found" & Length (Units)'Image & " units");

      return Units;
   end Get_Project_Analysis_Units_Vector;

end Gnatformat.Utils;
