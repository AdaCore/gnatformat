package body Gnatfmt.Documents.Builders is

   Document_Id : Integer := Integer'First;

   -----------------------
   -- Reset_Document_Id --
   -----------------------

   procedure Reset_Document_Id is
   begin
      Document_Id := Integer'First;
   end Reset_Document_Id;

   ---------------------
   -- New_Document_Id --
   ---------------------

   function New_Document_Id return Integer is
   begin
      return Result : constant Integer := Document_Id do
         Document_Id := @ + 1;
      end return;
   end New_Document_Id;

   ----------
   -- Text --
   ----------

   function Text
     (T : Ada.Strings.Unbounded.Unbounded_String)
         return Document_Type
   is
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind => Document_Text,
           Text => T,
           Id   => New_Document_Id);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Text;

   -----------------------
   --  To_Document_Type --
   -----------------------

   function List
     (Documents : Document_Type_Array)
      return Document_Type
   is
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind => Document_List,
           Id   => New_Document_Id,
           List => new Document_Type_Array'(Documents));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end List;

   -----------
   -- Align --
   -----------

   function Align
     (Kind     : Align_Kind_Type;
      Contents : Document_Type_Array)
      return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind            => Command_Align,
         Align_Kind      => Kind,
         Align_Contents  => List (Contents));
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Align;

   ------------------
   -- Break_Parent --
   ------------------

   function Break_Parent return Document_Type
   is
      Command       : constant Command_Type := (Kind => Command_Break_Parent);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Break_Parent;

   ------------
   -- Cursor --
   ------------

   function Cursor return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind => Command_Cursor, Place_Holder => New_Symbol);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Cursor;

   ----------
   -- Fill --
   ----------

   function Fill
     (Parts : Document_Type_Array)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind   => Command_Fill,
         Parts  => List (Parts));
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Fill;

   -----------
   -- Group --
   -----------

   function Group
     (Documents : Document_Type_Array;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind            => Command_Group,
         Id              => Options.Id,
         Group_Contents  => List (Documents),
         Break           => Options.Should_Break,
         Expanded_States => Options.Expanded_States);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Group;

   --------------
   -- If_Break --
   --------------

   function If_Break
     (Break_Contents : Document_Type;
      Flat_Contents  : Document_Type := No_Document;
      Options        : If_Break_Options_Type := No_If_Break_Options)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind              => Command_If_Break,
         If_Break_Group_Id => Options.Group_Id,
         Break_Contents    => Break_Contents,
         Flat_Contents     => Flat_Contents);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end If_Break;

   ------------
   -- Indent --
   ------------

   function Indent (Contents : Document_Type) return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind            => Command_Indent,
         Indent_Contents => Contents);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Indent;

   ---------------------
   -- Indent_If_Break --
   ---------------------

   function Indent_If_Break
     (Contents : Document_Type;
      Options : Indent_If_Break_Options_Type := No_Indent_If_Break_Options)
      return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind                     => Command_Indent_If_Break,
         Indent_If_Break_Contents => Contents,
         Indent_If_Break_Group_Id => Options.Group_Id,
         Negate                   => Options.Negate);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Indent_If_Break;

   -----------
   -- Label --
   -----------

   function Label
     (Text     : Ada.Strings.Unbounded.Unbounded_String;
      Contents : Document_Type)
      return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind           => Command_Label,
         Text           => Text,
         Label_Contents => Contents);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Label;

   ----------
   -- Line --
   ----------

   function Line return Document_Type
   is
      Command       : constant Command_Type_Access :=
        new Command_Type'(Kind => Command_Line);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
         (Kind    => Document_Command,
          Id      => New_Document_Id,
          Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Line;

   ---------------
   -- Soft_Line --
   ---------------

   function Soft_Line return Document_Type
   is
      Command       : constant Command_Type_Access :=
        new Command_Type'(Kind => Command_Soft_Line);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Soft_Line;

   ---------------
   -- Hard_Line --
   ---------------

   function Hard_Line return Document_Type
   is
      Command       : constant Command_Type_Access :=
        new Command_Type'(Kind => Command_Hard_Line);
      Hard_Line_Bare_Document      : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);
      Hard_Line_Document : constant Document_Type :=
        (Bare_Document => Hard_Line_Bare_Document);

   begin
      return List ([Hard_Line_Document, Break_Parent]);
   end Hard_Line;

   ------------------
   -- Literal_Line --
   ------------------

   function Literal_Line return Document_Type
   is
      Hard_Line_Command       : constant Command_Type_Access :=
        new Command_Type'(Kind => Command_Literal_Line);
      Hard_Line_Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Hard_Line_Command);
      Hard_Line_Document      : constant Document_Type :=
        (Bare_Document => Hard_Line_Bare_Document);

   begin
      return List ([Hard_Line_Document, Break_Parent]);
   end Literal_Line;

   ------------------------------------
   -- Hard_Line_Without_Break_Parent --
   ------------------------------------

   function Hard_Line_Without_Break_Parent return Document_Type
   is
      Command       : constant Command_Type_Access :=
        new Command_Type'(Kind => Command_Hard_Line);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Hard_Line_Without_Break_Parent;

   ---------------------------------------
   -- Lietral_Line_Without_Break_Parent --
   ---------------------------------------

   function Literal_Line_Without_Break_Parent return Document_Type
   is
      Command       : constant Command_Type_Access :=
        new Command_Type'(Kind => Command_Literal_Line);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Literal_Line_Without_Break_Parent;

   ---------------------
   -- New_Line_Suffix --
   ---------------------

   function Line_Suffix (Contents : Document_Type) return Document_Type
   is
      Command       : constant Command_Type_Access :=
        new Command_Type'
          (Kind                 => Command_Line_Suffix,
           Line_Suffix_Contents => Contents);
      Bare_Document : constant Bare_Document_Type_Access :=
        new Bare_Document_Type'
         (Kind    => Document_Command,
          Id      => New_Document_Id,
          Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Line_Suffix;

   ------------------------------
   -- New_Line_Suffix_Boundary --
   ------------------------------

   function Line_Suffix_Boundary return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind => Command_Line_Suffix_Boundary);
      Bare_Document : constant Bare_Document_Type :=
        (Kind    => Document_Command,
         Id      => New_Document_Id,
         Command => new Command_Type'(Command));

   begin
      return
        Document_Type'
          (Bare_Document =>
              new Bare_Document_Type'(Bare_Document));
   end Line_Suffix_Boundary;

   ----------
   -- Trim --
   ----------

   function Trim return Document_Type
   is
      Command        : constant Command_Type      := (Kind => Command_Trim);
      Bare_Document : constant Bare_Document_Type :=
        (Kind    => Document_Command,
         Id      => New_Document_Id,
         Command => new Command_Type'(Command));

   begin
      return
        Document_Type'
          (Bare_Document =>
             new Bare_Document_Type'(Bare_Document));
   end Trim;

   ----------
   -- Join --
   ----------

   function Join
     (Separator : Document_Type;
      Documents : Document_Type_Array)
      return Document_Type
   is
   begin
      if Documents'Length = 0 then
         return List ([]);

      elsif Documents'Length = 1 then
         declare
            Length : constant Positive := 2;
            Joined_Documents : Document_Type_Array (1 .. Length);
         begin
            Joined_Documents (1) := Documents (Documents'First);
            Joined_Documents (2) := Separator;
            return List (Joined_Documents);
         end;

      else
         declare
            Length : constant Positive :=
               Documents'Length + Documents'Length - 1;
            Joined_Documents : Document_Type_Array (1 .. Length);
         begin
            for J in 1 .. Length loop
               Joined_Documents (Documents'First - 2 * J - 1 - 1) :=
                 Documents (J);
            end loop;
            for J in 1 .. Length - 1 loop
               Joined_Documents (Documents'First + 2 * J - 1) := Separator;
            end loop;

            return List (Joined_Documents);
         end;
      end if;
   end Join;

end Gnatfmt.Documents.Builders;