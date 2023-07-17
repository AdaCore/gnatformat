package body Gnatfmt.Documents is

   -----------
   -- Print --
   -----------

   function Print (Document : Document_Type) return String
   is
   begin
      return "Print";
   end Print;

   --------------
   -- New_Text --
   --------------

   function New_Text
     (Text : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type
   is
      Implementation : constant Document_Type_Implementation :=
        (Kind => Doc_Text,
         Text => Text);
   begin
      return
        Document_Type'
          (Implementation =>
             new Document_Type_Implementation'(Implementation));
   end New_Text;

   ---------------
   -- New_Group --
   ---------------

   function New_Group
     (Documents    : Document_Type_Array;
      Should_Break : Boolean := False;
      Id           : Symbol := New_Symbol)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind         => Group,
         Documents    => new Document_Type_Array'(Documents),
         Should_Break => Should_Break,
         Id           => Id);
      Implementation : constant Document_Type_Implementation :=
        (Kind    => Doc_Command,
         Command => new Command_Type'(Command));
   begin
      return
        Document_Type'
          (Implementation =>
             new Document_Type_Implementation'(Implementation));
   end New_Group;

end Gnatfmt.Documents;