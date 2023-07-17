with Ada.Strings.Unbounded;

package Gnatfmt.Documents is
   type Document_Type is private;
   
   function Print (Document : Document_Type) return String;
   
   type Document_Type_Array is array (Positive range <>) of Document_Type;
   
   function New_Text
     (Text : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type;
   --  Creates a new Text Document Command

   function New_Group
     (Documents    : Document_Type_Array;
      Should_Break : Boolean := False;
      Id           : Symbol := New_Symbol)
      return Document_Type;
   --  Creates a new Group Document Command

private
   type Document_Type_Access is not null access all Document_Type;
   type Document_Type_Array_Access is not null access all Document_Type_Array;

   type Command_Kind is (Group, Fill);

   type Command_Type (Kind : Command_Kind := Group) is record
      case Kind is
         when Group | Fill =>
            Documents    : Document_Type_Array_Access;
            Should_Break : Boolean;
            Id           : Symbol;
      end case;
   end record;
   
   type Command_Type_Access is not null access all Command_Type;

   type Document_Kind is (Doc_Text, Doc_List, Doc_Command);
   
   type Document_Type_Implementation (Kind : Document_Kind) is record
      case Kind is
         when Doc_Text =>
            Text : Ada.Strings.Unbounded.Unbounded_String;
         when Doc_List =>
            List : Document_Type_Array_Access;
         when Doc_Command =>
            Command : Command_Type_Access;
      end case;
   end record;

   type Document_Type_Implementation_Access is
     not null access all Document_Type_Implementation;
   
   type Document_Type is record
      Implementation : Document_Type_Implementation_Access;
   end record;

end Gnatfmt.Documents;