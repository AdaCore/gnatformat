--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package that normalizes the casing of identifier occurrences, either to
--  match the spelling of their canonical defining name or to a lexical
--  casing convention (lowercase, uppercase or mixed).

with Ada.Strings.Unbounded;

with Gnatformat.Configuration;

with Libadalang.Analysis;

package Gnatformat.Identifier_Casing is

   function Normalize_Identifier_Casing
     (Unit   : Libadalang.Analysis.Analysis_Unit;
      Casing : Gnatformat.Configuration.Normalizing_Identifier_Casing_Kind)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns Unit's source text with identifier occurrences recased
   --  according to Casing:
   --
   --  * Definition: every occurrence - references, end labels and
   --    declarations - is rewritten to match the casing of its canonical
   --    defining name (requires name resolution).
   --
   --  * Lower, Upper and Mixed: every identifier token is recased
   --    lexically (my_var, MY_VAR and My_Var respectively), without name
   --    resolution.

   function Normalized_Unit
     (Unit   : Libadalang.Analysis.Analysis_Unit;
      Casing : Gnatformat.Configuration.Normalizing_Identifier_Casing_Kind)
      return Libadalang.Analysis.Analysis_Unit;
   --  Normalizes Unit's identifier casing (see Normalize_Identifier_Casing)
   --  and returns a unit parsed from the recased buffer. Returns Unit
   --  unchanged when the recased buffer fails to parse.

end Gnatformat.Identifier_Casing;
