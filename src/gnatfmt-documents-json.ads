package Gnatfmt.Documents.Json is

   function Serialize (Document : Document_Type) return String;
   --  TODO: Description

   function Deserialize (Document : String) return Document_Type;
   --  TODO: Description

end Gnatfmt.Documents.Json;