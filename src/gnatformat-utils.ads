------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
