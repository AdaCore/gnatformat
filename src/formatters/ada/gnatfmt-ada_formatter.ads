with Libadalang.Analysis;
with Prettier_Ada.Documents;
with Prettier_Ada.Generic_Formatters;

package Gnatfmt.Ada_Formatter is

   function Print_IR
     (Node : Libadalang.Analysis.Ada_Node)
      return Prettier_Ada.Documents.Document_Type;
   --  TODO: Description

   package Formatter is new
     Prettier_Ada.Generic_Formatters (Libadalang.Analysis.Ada_Node, Print_IR);

end Gnatfmt.Ada_Formatter;