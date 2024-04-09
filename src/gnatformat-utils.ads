--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;
with Gnatformat.Analysis_Unit_Vectors;

with Langkit_Support.Generic_API;
with Libadalang.Generic_API;

package Gnatformat.Utils is
   use Gnatformat.Analysis_Unit_Vectors;

   Language : Langkit_Support.Generic_API.Language_Id renames
     Libadalang.Generic_API.Ada_Lang_Id;
   --  This driver handles only the Ada language

   procedure Log_Progress
     (Current  : Natural;
      Total    : String;
      Message  : String);
   --  Logs a progress message with format '[X/Y] Message' where X is left
   --  padded with 0s so that it has the same length as Y.
   --  Example : '[001/581] Processing Foo'

   function Get_Project_Analysis_Units_Vector
     (Project_Filename : String)
      return Gnatformat.Analysis_Unit_Vectors.Vector;
   --  Gets all units of a project whose name is defined by Project_Filename.
   --  Project_Filename can either be a full path or a filename in the current
   --  directory.

   type Sources_List is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   function Get_Analysis_Units_Vector_From_Sources_List
     (Sources          : Sources_List;
      Project_Filename : String := "")
      return Gnatformat.Analysis_Unit_Vectors.Vector;
   --  Gets all units defined by Sources.
   --  If Project_Filename is defined, then uses it to create a unit provider.
   --  Project_Filename can either be a full path or a filename in the current
   --  directory.

end Gnatformat.Utils;
