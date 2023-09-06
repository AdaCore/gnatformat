package body Gnatfmt.Generic_Formatters is

   -----------
   -- Print --
   -----------

   function Print
     (Node    : Node_Type;
      Options : Gnatfmt.Documents.Format_Options_Type)
      return String
   is (Gnatfmt.Documents.Format (Print_IR (Node), Options));

end Gnatfmt.Generic_Formatters;