package body Gnatfmt.Generic_Formatters is

   -----------
   -- Print --
   -----------

   function Print (Node : Node_Type) return String is
      (Gnatfmt.Documents.Print (Print_IR (Node)));

end Gnatfmt.Generic_Formatters;