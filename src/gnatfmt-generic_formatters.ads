with Gnatfmt.Documents;

generic
   type Node_Type is private;
   with function Print_IR
     (Node : Node_Type)
      return Gnatfmt.Documents.Document_Type;
package Gnatfmt.Generic_Formatters is
   function Print (Node : Node_Type) return String;
end Gnatfmt.Generic_Formatters;