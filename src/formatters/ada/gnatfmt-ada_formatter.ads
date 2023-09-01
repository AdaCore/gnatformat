with Libadalang.Analysis;

package Gnatfmt.Ada_Formatter is

   function Print_IR
     (Node : Libadalang.Analysis.Ada_Node)
      return Gnatfmt.Documents.Document_Type;
   --  TODO: Description

   package Formatter is new
     Gnatfmt.Generic_Formatters (Libadalang.Analysis.Ada_Node, Print_IR);

end Gnatfmt.Ada_Formatter;