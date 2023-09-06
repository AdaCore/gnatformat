package Gnatfmt.Documents.Builders is

   No_Document : constant Document_Type;

   No_Symbol : constant Symbol_Type;

   function New_Document_Id return Integer;
   --  TODO: Description

   procedure Reset_Document_Id;
   --  TODO: Description

   function Text
     (T : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type;
   --  Convert text into a Document_Type object

   function List
     (Documents : Document_Type_Array)
      return Document_Type;
   --  Convert an array of Document_Type objects into a Document_Type object

   function Align
     (Kind     : Align_Kind_Type;
      Contents : Document_Type_Array)
      return Document_Type;
   --  Creates a new Align Document Command

   function Break_Parent return Document_Type;
   --  Creates a new Break_Parent Document Command

   function Cursor return Document_Type;
   --  Creates a new Cursor Document Command

   function Fill
     (Parts : Document_Type_Array)
      return Document_Type;
   --  Creates a new Fill Document Command

   type Group_Options_Type is record
      Should_Break    : Boolean;
      Id              : Symbol_Type;
      Expanded_States : Document_Type;
   end record;

   No_Group_Options : constant Group_Options_Type;

   function Group
     (Documents : Document_Type_Array;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type;
   --  Creates a new Group Document Command

   type If_Break_Options_Type is record
      Group_Id : Symbol_Type;
   end record;

   No_If_Break_Options : constant If_Break_Options_Type;

   function If_Break
     (Break_Contents : Document_Type;
      Flat_Contents  : Document_Type := No_Document;
      Options        : If_Break_Options_Type := No_If_Break_Options)
      return Document_Type;
   --  Creates a new If_Break Document Command
   --  TODO: Confirm if Break_Contents and Flat_Contents should be a
   --  a Document_Type_Array.

   function Indent (Contents : Document_Type) return Document_Type;
   --  Creates a new Indent Document Command
   --  TODO: Confirm if Break_Contents and Flat_Contents should be a
   --  a Document_Type_Array.

   type Indent_If_Break_Options_Type is record
      Group_Id : Symbol_Type;
      Negate   : Boolean;
   end record;

   No_Indent_If_Break_Options : constant Indent_If_Break_Options_Type;

   function Indent_If_Break
     (Contents : Document_Type;
      Options : Indent_If_Break_Options_Type := No_Indent_If_Break_Options)
      return Document_Type;
   --  Creates a new Indent_If_Break Document Command

   function Label
     (Text     : Ada.Strings.Unbounded.Unbounded_String;
      Contents : Document_Type)
      return Document_Type;
   --  Creates a new Label Document Command

   function Line return Document_Type;
   --  Creates a new Line Document Command

   function Soft_Line return Document_Type;
   --  Creates a new Line Document Command

   function Hard_Line return Document_Type;
   --  Creates a new Line Document Command

   function Literal_Line return Document_Type;
   --  Creates a new Line Document Command

   function Hard_Line_Without_Break_Parent return Document_Type;
   --  Creates a new Line Document Command

   function Literal_Line_Without_Break_Parent return Document_Type;
   --  Creates a new Line Document Command

   function Line_Suffix (Contents : Document_Type) return Document_Type;
   --  Creates a new Line_Suffix Document Command

   function Line_Suffix_Boundary return Document_Type;
   --  Creates a new Line_Suffix_Boundary Document Command

   function Trim return Document_Type;
   --  Creates a new Trim Document Command

   function Join
     (Separator : Document_Type;
      Documents : Document_Type_Array)
      return Document_Type;
   --  Join an array of Documents with a Separator

private

   No_Document : constant Document_Type := Gnatfmt.Documents.No_Document;

   No_Symbol : constant Symbol_Type := Gnatfmt.Documents.No_Symbol;

   No_Group_Options : constant Group_Options_Type :=
     (Should_Break    => False,
      Id              => No_Symbol,
      Expanded_States => No_Document);

   No_Indent_If_Break_Options : constant Indent_If_Break_Options_Type :=
     (Group_Id => No_Symbol, Negate => False);

   No_If_Break_Options : constant If_Break_Options_Type :=
     (Group_Id => No_Symbol);

end Gnatfmt.Documents.Builders;
