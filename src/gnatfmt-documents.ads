with Ada.Strings.Unbounded;

private with Ada.Containers;

package Gnatfmt.Documents is

   type Indentation_Kind is (Spaces, Tabs);

   type Indentation_Options_Type is record
      Kind  : Indentation_Kind := Spaces;
      Width : Natural := 3;
   end record;

   type Format_Options_Type is record
      Width       : Natural := 79;
      Indentation : Indentation_Options_Type := (Spaces, 3);
   end record;

   type Document_Type is private;

   function Format
     (Document : Document_Type;
      Options : Format_Options_Type)
      return String;
   --  TODO: Description

   type Document_Type_Array is array (Positive range <>) of Document_Type;

   type Symbol_Type is private;

   function New_Symbol return Symbol_Type;
   --  TODO: Description

   --  TODO: Align_Data_Type is necesary for the the Document_Type and its
   --  builders but it does not belong to this package. Make this private and
   --  private another type in the builders
   type Align_Kind_Type is (Width, Text, Dedent, Dedent_To_Root, Root, None);

   type Align_Data_Type (Kind : Align_Kind_Type := None) is record
      case Kind is
         when Width =>
            N : Integer; -- Number of spaces or tabs
         when Text =>
            T : Ada.Strings.Unbounded.Unbounded_String;
         when Dedent | Dedent_To_Root | Root | None =>
            null;
      end case;
   end record;

private

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type;
   --  TODO: Description

   function Hash (Document : Document_Type) return Ada.Containers.Hash_Type;
   --  TODO: Description

   type Document_Type_Array_Access is access all Document_Type_Array;

   type Command_Kind_Type is
     (Command_Align,
      Command_Break_Parent,
      Command_Cursor,
      Command_Fill,
      Command_Group,
      Command_If_Break,
      Command_Indent,
      Command_Indent_If_Break,
      Command_Label,
      Command_Line,
      --  Command_Soft_Line,
      --  Command_Hard_Line,
      --  Command_Literal_Line,
      Command_Line_Suffix,
      Command_Line_Suffix_Boundary,
      Command_Trim);

   type Command_Type (Kind : Command_Kind_Type) is record
      case Kind is
         when Command_Align =>
            Align_Data     : Align_Data_Type;
            Align_Contents : Document_Type;

         when Command_Break_Parent =>
            null;

         when Command_Cursor =>
            Place_Holder : Symbol_Type;

         when Command_Fill =>
            --  TODO: Should this be a array of Document_Type?
            --  If not, should it have a dynamic predicate to enforce that
            --  Parts.Bare_Document.Kind = Document_List
            Parts : Document_Type;

         when Command_Group =>
            Id              : Symbol_Type;
            Group_Contents  : Document_Type;
            Break           : Boolean;
            --  TODO: Can we make this optional?
            Expanded_States : Document_Type;

         when Command_If_Break =>
            If_Break_Group_Id : Symbol_Type;
            --  TODO: Should any of the following be optional?
            Break_Contents    : Document_Type;
            Flat_Contents     : Document_Type;

         when Command_Indent =>
            Indent_Contents : Document_Type;

         when Command_Indent_If_Break =>
            Indent_If_Break_Contents : Document_Type;
            Indent_If_Break_Group_Id : Symbol_Type;
            Negate                   : Boolean := False;

         when Command_Label =>
            Text           : Ada.Strings.Unbounded.Unbounded_String;
            Label_Contents : Document_Type;

         when Command_Line =>
            Literal : Boolean;
            Soft    : Boolean;
            Hard    : Boolean;

         --  when Command_Soft_Line
         --       | Command_Hard_Line
         --       | Command_Literal_Line =>
         --     null;

         when Command_Line_Suffix =>
            Line_Suffix_Contents : Document_Type;

         when Command_Line_Suffix_Boundary =>
            null;

         when Command_Trim =>
            null;
      end case;
   end record;

   --  TODO: Try to make this this not null
   type Command_Type_Access is access all Command_Type;

   type Document_Kind_Type is (Document_Text, Document_List, Document_Command);

   type Bare_Document_Type (Kind : Document_Kind_Type) is record
      Id : Integer;
      case Kind is
         when Document_Text =>
            Text : Ada.Strings.Unbounded.Unbounded_String;
         when Document_List =>
            List : Document_Type_Array_Access;
         when Document_Command =>
            --  TODO: We need an access here becuase Propagate_Breaks relies
            --  on a vector of mutable Command_Type objects. Investigate if
            --  there's a way of making these aliased and using a vector
            --  of an access to a Command_Type object.
            Command : Command_Type_Access;
      end case;
   end record;

   type Bare_Document_Type_Access is access all Bare_Document_Type;

   type Document_Type is record
      Bare_Document : Bare_Document_Type_Access := null;
   end record;

   No_Document : constant Document_Type := (Bare_Document =>  null);

   type Symbol_Type is new Integer;

   No_Symbol : constant Symbol_Type := Symbol_Type (Integer'First);

end Gnatfmt.Documents;
