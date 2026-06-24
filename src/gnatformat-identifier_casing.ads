--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package that normalizes the casing of identifier occurrences so that they
--  match the spelling of their canonical defining name.

with Ada.Strings.Unbounded;

with Libadalang.Analysis;

package Gnatformat.Identifier_Casing is

   function Normalize_Identifier_Casing
     (Unit : Libadalang.Analysis.Analysis_Unit)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns Unit's source text with every identifier occurrence -
   --  references, end labels and declarations - rewritten to match the casing
   --  of its canonical defining name.

   function Normalized_Unit
     (Unit : Libadalang.Analysis.Analysis_Unit)
      return Libadalang.Analysis.Analysis_Unit;
   --  Normalizes Unit's identifier casing (see Normalize_Identifier_Casing)
   --  and returns a unit parsed from the recased buffer. Returns Unit
   --  unchanged when the recased buffer fails to parse.

end Gnatformat.Identifier_Casing;
