--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

package Gnatformat.Helpers is

   function Read_To_Unbounded_String
     (Path : String) return Ada.Strings.Unbounded.Unbounded_String;
   --  Reads the contents of the file at the given Path and returns it as an
   --  Ada.Strings.Unbounded.Unbounded_String.

   procedure Write
     (Path : String; Contents : Ada.Strings.Unbounded.Unbounded_String);
   --  Writes Contents as the entire contents of a file given by Path

end Gnatformat.Helpers;
