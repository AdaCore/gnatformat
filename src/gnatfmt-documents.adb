with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Strings;

with Gnatfmt.Documents.Builders;
with Gnatfmt.Optionals;
with Gnatfmt.Hashes;

package body Gnatfmt.Documents is

   Current_Symbol : Symbol_Type := Symbol_Type (Integer'First + 1);

   ----------------
   -- New_Symbol --
   ----------------

   function New_Symbol return Symbol_Type is
   begin
      return S : constant Symbol_Type := Current_Symbol do
         Current_Symbol := Current_Symbol + 1;
      end return;
   end New_Symbol;

   package Optional_Booleans is new
     Gnatfmt.Optionals.Generic_Optional_Types (Boolean);

   subtype Optional_Boolean is Optional_Booleans.Generic_Optional_Type;

   package Document_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Document_Type);

   subtype Document_Type_Vector is
     Document_Type_Vectors.Vector;

   package Group_Command_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Command_Type_Access);

   subtype Group_Command_Type_Vector is Group_Command_Type_Vectors.Vector;

   package Document_Type_Hashed_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Document_Type,
        Hash                => Hash,
        Equivalent_Elements => "=");

   subtype Document_Type_Hashed_Set is
     Document_Type_Hashed_Sets.Set;

   type Mode_Kind is (Mode_Break, Mode_Flat);

   type Indentation_Data_Kind is (Indent, String_Align, Number_Align, Dedent);

   type Indentation_Data_Type (Kind : Indentation_Data_Kind := Indent) is
      record
         case Kind is
            when Indent | Dedent =>
               null;
            when String_Align =>
               T : Ada.Strings.Unbounded.Unbounded_String;
            when Number_Align =>
               N : Integer;
         end case;
      end record;

   package Indentation_Data_Vectors is new
     Ada.Containers.Vectors (Natural, Indentation_Data_Type);

   subtype Indentation_Data_Vector is Indentation_Data_Vectors.Vector;

   type Indentation_Type;

   type Indentation_Type_Access is access Indentation_Type;

   type Indentation_Type is record
      Value  : Ada.Strings.Unbounded.Unbounded_String;
      Length : Natural;
      Queue  : Indentation_Data_Vector;
      Root   : Indentation_Type_Access;
   end record;

   type Print_Command_Type is record
      Indentation : Indentation_Type;
      Mode        : Mode_Kind;
      Document    : Document_Type;
   end record;

   package Print_Command_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Print_Command_Type);

   subtype Print_Command_Type_Vector is Print_Command_Type_Vectors.Vector;

   package Symbol_To_Mode_Maps is new
     Ada.Containers.Hashed_Maps (Symbol_Type, Mode_Kind, Hash, "=");

   subtype Symbol_To_Mode_Map is Symbol_To_Mode_Maps.Map;

   procedure Break_Parent_Group
     (Group_Stack : in out Group_Command_Type_Vector);
   --  TODO: Description

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Natural;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map;
      Must_Be_Flat    : Boolean := False)
      return Boolean;
   --  TODO: Description

   function Generate_Indentation
     (From    : Indentation_Type;
      Data    : Indentation_Data_Type;
      Options : Indentation_Options_Type)
      return Indentation_Type;
   --  TODO: Description

   function Get_Document_Parts
     (Document : Document_Type)
      return Document_Type_Array_Access;
   --  TODO: Add the following contract:
   --  with Pre => Document.Kind = Document_List
   --              or else (Document.Kind = Document_Command
   --                       and then Document.Bare_Document.Command.Kind
   --                                = Command_Fill);
   --  TODO: Description

   function Make_Align
     (From       : Indentation_Type;
      Align_Data : Align_Data_Type;
      Options    : Format_Options_Type)
      return Indentation_Type;
   --  TODO: Description

   function Make_Indentation
     (From    : Indentation_Type;
      Options : Indentation_Options_Type)
      return Indentation_Type;
   --  TODO: Description

   function Last
     (Document_List : Document_Type_Array_Access)
      return Document_Type;
   --  TODO: Description

   procedure Propagate_Breaks (Document : Document_Type);
   --  TODO: Description

   function Root_Indent return Indentation_Type;
   --  TODO: Description

   procedure Traverse_Document
     (Document                           : Document_Type;
      On_Enter                           :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      On_Exit                            :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      Should_Traverse_Conditional_Groups : Boolean := False);
   --  TODO: Description
   --  TODO: Refactor so that Optional_Boolean is not needed

   function Trim
     (Text : in out Ada.Strings.Unbounded.Unbounded_String)
      return Natural;
   --  TODO: Description

   ------------------------
   -- Break_Parent_Group --
   ------------------------

   procedure Break_Parent_Group
     (Group_Stack : in out Group_Command_Type_Vector) is
      use type Ada.Containers.Count_Type;

   begin
      if Group_Stack.Length > 0 then
         declare
            Parent_Group : constant Group_Command_Type_Vectors.Reference_Type :=
              Group_Stack.Reference (Group_Stack.Last);

         begin
            if (Parent_Group.Expanded_States /= No_Document
                and then Parent_Group.Expanded_States.Bare_Document.List /= null
                and then Parent_Group.Expanded_States.Bare_Document.List'Length /= 0)
              and not Parent_Group.Break
            then
               --  TODO: Why should this be "propagated"?
               Parent_Group.Break := True;
            end if;
         end;
      end if;
   end Break_Parent_Group;

   ----------
   -- Fits --
   ----------

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Natural;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map;
      Must_Be_Flat    : Boolean := False)
      return Boolean
   is
      type Fit_Command_Type is record
         Mode     : Mode_Kind;
         Document : Document_Type;
      end record;

      package Fit_Command_Type_Vectors is new
        Ada.Containers.Vectors (Positive, Fit_Command_Type);

      subtype Fit_Command_Type_Vector is Fit_Command_Type_Vectors.Vector;

      function To_Fit_Command_Type
        (Print_Command : Print_Command_Type)
         return Fit_Command_Type;
      --  TODO: Description

      -------------------------
      -- To_Fit_Command_Type --
      -------------------------

      function To_Fit_Command_Type
        (Print_Command : Print_Command_Type)
         return Fit_Command_Type
      is (Fit_Command_Type'(Print_Command.Mode, Print_Command.Document));

      function "+" (Print_Comment : Print_Command_Type) return Fit_Command_Type
        renames To_Fit_Command_Type;

      Remaining_Width : Integer := Width;
      Rest_Idx        : Print_Command_Type_Vectors.Extended_Index :=
        Rest_Commands.Last_Index;
      Fit_Commands    : Fit_Command_Type_Vector := [+Next];

      Current_Line : Ada.Strings.Unbounded.Unbounded_String;

      Current_Has_Line_Suffix : Boolean := Has_Line_Suffix;

   begin
      if Remaining_Width = Natural'Last then
         return True;
      end if;

      while Remaining_Width >= 0 loop
         if Fit_Commands.Is_Empty then
            if Rest_Idx < Rest_Commands.First_Index then
               return True;
            end if;

            Fit_Commands.Append (+Rest_Commands.Element (Rest_Idx));
            Rest_Idx := @ - 1;

         else
            declare
               Current_Fit_Command : constant Fit_Command_Type :=
                  Fit_Commands.Last_Element;
               Mode                : Mode_Kind
                 renames Current_Fit_Command.Mode;
               Document            : Document_Type
                 renames Current_Fit_Command.Document;

            begin
               Fit_Commands.Delete_Last;

               case Document.Bare_Document.Kind is
                  when Document_Text =>
                     Ada.Strings.Unbounded.Append
                       (Current_Line, Document.Bare_Document.Text);
                     Remaining_Width :=
                       @ - Ada.Strings.Unbounded.Length
                             (Document.Bare_Document.Text);

                  when Document_List =>
                     for Child_Document of
                        reverse Document.Bare_Document.List.all
                     loop
                        Fit_Commands.Append
                          (Fit_Command_Type'(Mode, Child_Document));
                     end loop;

                  when Document_Command =>
                     case Document.Bare_Document.Command.Kind is
                        when Command_Fill =>
                           for Child_Document of
                              Get_Document_Parts (Document).all
                           loop
                              Fit_Commands.Append
                                (Fit_Command_Type'(Mode, Child_Document));
                           end loop;

                        when Command_Indent =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Indent_Contents));

                        when Command_Align =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Align_Contents));

                        when Command_Indent_If_Break =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Indent_If_Break_Contents));

                        when Command_Label =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Label_Contents));

                        when Command_Trim =>
                           Remaining_Width := @ + Trim (Current_Line);

                        when Command_Group =>
                           if Must_Be_Flat
                             and Document.Bare_Document.Command.Break
                           then
                              return False;
                           end if;
                           declare
                              Group_Mode : constant Mode_Kind :=
                                (if Document.Bare_Document.Command.Break then
                                   Mode_Break
                                 else
                                   Mode);
                              Contents : constant Document_Type :=
                                (if Document
                                      .Bare_Document
                                      .Command
                                      .Expanded_States
                                      .Bare_Document /= null
                                   and Group_Mode = Mode_Break
                                 then
                                   Last
                                     (Document
                                       .Bare_Document
                                       .Command
                                       .Expanded_States
                                       .Bare_Document
                                       .List)
                                 else
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Group_Contents);
                           begin
                              Fit_Commands.Append
                                (Fit_Command_Type'(Group_Mode, Contents));
                           end;

                        when Command_If_Break =>
                           declare
                              Group_Mode : constant Mode_Kind :=
                                (if Document
                                      .Bare_Document
                                      .Command
                                      .If_Break_Group_Id
                                    /= No_Symbol
                                 then
                                   (if Group_Mode_Map.Contains
                                         (Document
                                            .Bare_Document
                                            .Command
                                            .If_Break_Group_Id)
                                    then
                                      Group_Mode_Map.Element
                                        (Document
                                           .Bare_Document
                                           .Command
                                           .If_Break_Group_Id)
                                    else
                                      Mode_Flat)
                                 else
                                   Mode);
                              Contents : constant Document_Type :=
                                (if Group_Mode = Mode_Break then
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Break_Contents
                                 else
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Flat_Contents);
                           begin
                              if Contents.Bare_Document /= null then
                                 Fit_Commands.Append
                                   (Fit_Command_Type'(Mode, Contents));
                              end if;
                           end;

                        when Command_Line =>
                           if Mode = Mode_Break
                             or Document.Bare_Document.Command.Hard
                           then
                              return True;
                           end if;
                           if not Document.Bare_Document.Command.Soft then
                              Ada.Strings.Unbounded.Append (Current_Line, " ");
                              Remaining_Width := @ - 1;
                           end if;

                        when Command_Line_Suffix =>
                           Current_Has_Line_Suffix := True;

                        when Command_Line_Suffix_Boundary =>
                           if Current_Has_Line_Suffix then
                              return False;
                           end if;

                        when Command_Break_Parent
                             | Command_Cursor =>
                           null;

                     end case;
               end case;
            end;
         end if;
      end loop;

      return False;
   end Fits;

   ----------
   -- Hash --
   ----------

   function Hash
     (Document : Document_Type)
      return Ada.Containers.Hash_Type
   is
      function Bare_Document_Access_Type_Hash is new
        Gnatfmt.Hashes.Hash_Access
          (Bare_Document_Type, Bare_Document_Type_Access);
   begin
      return Bare_Document_Access_Type_Hash (Document.Bare_Document);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Symbol));

   ------------
   -- Format --
   ------------

   function Format
     (Document : Document_Type;
      Options  : Format_Options_Type)
      return String
   is

      use type Ada.Containers.Count_Type;

      Group_Mode_Map : Symbol_To_Mode_Map;

      Pos : Natural := 0;

      Should_Remeasure : Boolean := False;

      Line_Suffix : Print_Command_Type_Vector := [];

      Print_Commands : Print_Command_Type_Vector :=
         [Print_Command_Type'
            (Root_Indent,
             Mode_Break,
             Document)];

      Result : Ada.Strings.Unbounded.Unbounded_String;

   begin
      Propagate_Breaks (Document);

      while Print_Commands.Length > 0 loop
         declare
            Print_Command : constant Print_Command_Type :=
              Print_Commands.Last_Element;
            Indentation   : Indentation_Type renames Print_Command.Indentation;
            Mode          : Mode_Kind renames Print_Command.Mode;
            Document      : Document_Type renames Print_Command.Document;

         begin
            Print_Commands.Delete_Last;

            case Document.Bare_Document.Kind is
               when Document_Text =>
                  declare
                     --  TODO: Do '\n' convertions to '\r\n\' if necessary
                     Formatted :
                       constant Ada.Strings.Unbounded.Unbounded_String :=
                         Document.Bare_Document.Text;

                  begin
                     Ada.Strings.Unbounded.Append (Result, Formatted);
                     if Print_Commands.Length > 0 then
                        Pos := @ + Ada.Strings.Unbounded.Length (Formatted);
                     end if;
                  end;

               when Document_List =>
                  for Command of
                     reverse Document.Bare_Document.List.all
                  loop
                     Print_Commands.Append
                       (Print_Command_Type'(Indentation, Mode, Command));
                  end loop;

               when Document_Command =>
                  case Document.Bare_Document.Command.Kind is
                     when Command_Cursor =>
                        raise Program_Error;

                     when Command_Indent =>
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Indentation
                                (Indentation, Options.Indentation),
                              Mode,
                              Document.Bare_Document.Command.Indent_Contents));

                     when Command_Align =>
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Align
                                (Indentation,
                                 Document.Bare_Document.Command.Align_Data,
                                 Options),
                              Mode,
                              Document.Bare_Document.Command.Align_Contents));

                     when Command_Trim =>
                        Pos := @ - Trim (Result);

                     when Command_Group =>
                        declare
                           procedure Process_Mode_Break;
                           --  TODO

                           procedure Process_Mode_Flat;
                           --  TODO

                           ------------------------
                           -- Process_Mode_Break --
                           ------------------------

                           procedure Process_Mode_Break is
                              Next : constant Print_Command_Type :=
                                (Indentation => Indentation,
                                 Mode        => Mode_Flat,
                                 Document    =>
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Group_Contents);
                              Remainder : constant Natural :=
                                Options.Width - Pos;
                              Has_Line_Suffix : constant Boolean :=
                                Line_Suffix.Length > 0;

                           begin
                              Should_Remeasure := False;

                              if not Document.Bare_Document.Command.Break
                                 and then
                                   Fits
                                     (Next,
                                      Print_Commands,
                                      Remainder,
                                      Has_Line_Suffix,
                                      Group_Mode_Map)
                              then
                                 Print_Commands.Append (Next);

                              else
                                 if Document
                                      .Bare_Document
                                      .Command
                                      .Expanded_States
                                      .Bare_Document
                                    /= null
                                 then
                                    declare
                                       Group_Contents : Document_Type renames
                                         Document
                                           .Bare_Document
                                           .Command
                                           .Group_Contents;
                                       Expanded_States : Document_Type_Array_Access renames
                                         Document
                                           .Bare_Document
                                           .Command
                                           .Expanded_States
                                           .Bare_Document.List;
                                       Most_Expanded : constant Document_Type :=
                                         Expanded_States (Expanded_States'Last);

                                    begin
                                       if Document.Bare_Document.Command.Break then
                                          Print_Commands.Append
                                            (Print_Command_Type'
                                               (Indentation, Mode_Break, Most_Expanded));

                                       else
                                          for J in
                                             Expanded_States'First .. Expanded_States'Last - 1
                                          loop
                                             declare
                                                State         : constant Document_Type :=
                                                  Expanded_States (J);
                                                Print_Command : constant Print_Command_Type :=
                                                  (Indentation, Mode_Flat, State);
                                             begin
                                                if Fits
                                                     (Print_Command,
                                                      Print_Commands,
                                                      Remainder,
                                                      Has_Line_Suffix,
                                                      Group_Mode_Map)
                                                then
                                                   Print_Commands.Append
                                                     (Print_Command_Type'
                                                        (Indentation, Mode_Break, Group_Contents));
                                                end if;
                                             end;
                                          end loop;

                                          Print_Commands.Append
                                            (Print_Command_Type'
                                               (Indentation,
                                                Mode_Break,
                                                Most_Expanded));
                                       end if;
                                    end;

                                 else
                                    Print_Commands.Append
                                      (Print_Command_Type'
                                         (Indentation,
                                          Mode_Break,
                                          Document.Bare_Document.Command.Group_Contents));
                                 end if;
                              end if;
                           end Process_Mode_Break;

                           -----------------------
                           -- Process_Mode_Flat --
                           -----------------------

                           procedure Process_Mode_Flat is
                           begin
                              if not Should_Remeasure then
                                 Print_Commands.Append
                                    (Print_Command_Type'
                                       (Indentation,
                                        (if Document.Bare_Document.Command.Break then Mode_Break
                                         else Mode_Flat),
                                        Document.Bare_Document.Command.Group_Contents));
                              else
                                 Process_Mode_Break;
                              end if;
                           end Process_Mode_Flat;

                        begin
                           case Mode is
                              when Mode_Flat =>
                                 Process_Mode_Flat;

                              when Mode_Break =>
                                 Process_Mode_Break;
                           end case;

                           if Document.Bare_Document.Command.Id /= No_Symbol then
                              Group_Mode_Map.Include (Document.Bare_Document.Command.Id, Mode);
                           end if;
                        end;

                     when Command_Fill =>
                        declare
                           Remainder : constant Natural := Options.Width - Pos;
                           Parts     : Document_Type_Array_Access renames
                             Document.Bare_Document.Command.Parts.Bare_Document.List;
                        begin
                           if Parts'Length = 1 then
                              declare
                                 Content_Flat_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Flat, Parts (Parts'First));
                                 Content_Break_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Break, Parts (Parts'First));
                                 Content_Fits : constant Boolean :=
                                   Fits
                                     (Content_Flat_Command,
                                      [],
                                      Remainder,
                                      Line_Suffix.Length > 0,
                                      Group_Mode_Map,
                                      True);

                              begin
                                 if Content_Fits then
                                    Print_Commands.Append (Content_Flat_Command);

                                 else
                                    Print_Commands.Append (Content_Break_Command);
                                 end if;
                              end;

                           elsif Parts'Length = 2 then
                              declare
                                 Content_Flat_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Flat, Parts (Parts'First));
                                 Content_Break_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Break, Parts (Parts'First));
                                 White_Flat_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Flat, Parts (Parts'First + 1));
                                 White_Break_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Break, Parts (Parts'First + 1));
                                 Content_Fits : constant Boolean :=
                                   Fits
                                     (Content_Flat_Command,
                                      [],
                                      Remainder,
                                      Line_Suffix.Length > 0,
                                      Group_Mode_Map,
                                      True);

                              begin
                                 if Content_Fits then
                                    Print_Commands.Append (White_Flat_Command);
                                    Print_Commands.Append (Content_Flat_Command);

                                 else
                                    Print_Commands.Append (White_Break_Command);
                                    Print_Commands.Append (Content_Break_Command);
                                 end if;
                              end;

                           elsif Parts'Length /= 0 then
                              declare
                                 Content_Flat_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Flat, Parts (Parts'First));
                                 Content_Break_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Break, Parts (Parts'First));
                                 White_Flat_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Flat, Parts (Parts'First + 1));
                                 White_Break_Command : constant Print_Command_Type :=
                                    (Indentation, Mode_Break, Parts (Parts'First + 1));
                                 Content_Fits : constant Boolean :=
                                   Fits
                                     (Content_Flat_Command,
                                      [],
                                      Remainder,
                                      Line_Suffix.Length > 0,
                                      Group_Mode_Map,
                                      True);
                                 Remaining_Parts : constant Document_Type_Array :=
                                   Parts (Parts'First + 2 .. Parts'Last);
                                 Remaining_Print_Command  : constant Print_Command_Type :=
                                   (Indentation,
                                    Mode,
                                    Gnatfmt.Documents.Builders.Fill (Remaining_Parts));
                                 First_And_Second_Content_Flat_Command :
                                   constant Print_Command_Type :=
                                     (Indentation,
                                      Mode_Flat,
                                      Gnatfmt.Documents.Builders.List
                                        (Parts (Parts'First .. Parts'First + 2)));
                                 First_And_Second_Content_Firts : constant Boolean :=
                                   Fits
                                     (First_And_Second_Content_Flat_Command,
                                      [],
                                      Remainder,
                                      Line_Suffix.Length > 0,
                                      Group_Mode_Map,
                                      True);

                              begin
                                 if First_And_Second_Content_Firts then
                                    Print_Commands.Append (Remaining_Print_Command);
                                    Print_Commands.Append (White_Flat_Command);
                                    Print_Commands.Append (Content_Flat_Command);

                                 elsif Content_Fits then
                                    Print_Commands.Append (Remaining_Print_Command);
                                    Print_Commands.Append (White_Break_Command);
                                    Print_Commands.Append (Content_Flat_Command);

                                 else
                                    Print_Commands.Append (Remaining_Print_Command);
                                    Print_Commands.Append (White_Break_Command);
                                    Print_Commands.Append (Content_Break_Command);
                                 end if;
                              end;

                           end if;
                        end;

                     when Command_If_Break =>
                        declare
                           Group_Mode : constant Mode_Kind :=
                             (if Document.Bare_Document.Command.If_Break_Group_Id /= No_Symbol
                                and then Group_Mode_Map.Contains
                                           (Document.Bare_Document.Command.If_Break_Group_Id)
                              then
                                 Group_Mode_Map.Element
                                   (Document.Bare_Document.Command.If_Break_Group_Id)
                              else
                                 Mode);

                        begin
                           case Group_Mode is
                              when Mode_Break =>
                                 declare
                                    Break_Contents : constant Document_Type :=
                                      Document.Bare_Document.Command.Break_Contents;

                                 begin
                                    if Break_Contents.Bare_Document /= null then
                                       Print_Commands.Append
                                         (Print_Command_Type'
                                            (Indentation, Group_Mode, Break_Contents));
                                    end if;
                                 end;

                              when Mode_Flat =>
                                 declare
                                    Flat_Contents : Document_Type
                                      renames Document.Bare_Document.Command.Flat_Contents;

                                 begin
                                    if Flat_Contents.Bare_Document /= null then
                                       Print_Commands.Append
                                         (Print_Command_Type'
                                            (Indentation, Group_Mode, Flat_Contents));
                                    end if;
                                 end;

                           end case;
                        end;

                     when Command_Indent_If_Break  =>
                        declare
                           Group_Mode : constant Mode_Kind :=
                             (if Document.Bare_Document.Command.If_Break_Group_Id /= No_Symbol
                                and then Group_Mode_Map.Contains
                                           (Document.Bare_Document.Command.If_Break_Group_Id)
                              then
                                 Group_Mode_Map.Element
                                   (Document.Bare_Document.Command.If_Break_Group_Id)
                              else
                                 Mode);
                           Contents   : constant Document_Type :=
                              (if Document.Bare_Document.Command.Negate
                               then Document.Bare_Document.Command.Indent_If_Break_Contents
                               else Gnatfmt.Documents.Builders.Indent
                                      (Document
                                         .Bare_Document
                                         .Command
                                         .Indent_If_Break_Contents));

                        begin
                           if Contents.Bare_Document /= null then
                              Print_Commands.Append
                                (Print_Command_Type'(Indentation, Group_Mode, Contents));
                           end if;
                        end;

                     when Command_Line_Suffix =>
                        Line_Suffix.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode,
                              Document.Bare_Document.Command.Line_Suffix_Contents));

                     when Command_Line_Suffix_Boundary =>
                        if not Line_Suffix.Is_Empty then
                           Print_Commands.Append
                             (Print_Command_Type'
                                (Indentation,
                                 Mode,
                                 Gnatfmt.Documents.Builders.Hard_Line_Without_Break_Parent));
                        end if;

                     when Command_Line =>
                        declare
                           procedure Process_Mode_Break;
                           --  TODO

                           procedure Process_Mode_Flat;
                           --  TODO

                           ------------------------
                           -- Process_Mode_Break --
                           ------------------------

                           procedure Process_Mode_Break is
                           begin
                              if not Line_Suffix.Is_Empty then
                                 Print_Commands.Append
                                   (Print_Command_Type'(Indentation, Mode, Document));
                                 Print_Commands.Append_Vector (Line_Suffix);
                                 Line_Suffix.Clear;

                              else
                                 if Document.Bare_Document.Command.Literal then
                                    if Indentation.Root /= null then
                                       Ada.Strings.Unbounded.Append (Result, Indentation.Root.Value);
                                       Pos := Indentation.Root.Length;

                                    else
                                       Ada.Strings.Unbounded.Append
                                         (Result, Ada.Characters.Latin_1.LF);
                                       Pos := 0;
                                    end if;

                                 else
                                    Pos := @ - Trim (Result);
                                    Ada.Strings.Unbounded.Append
                                      (Result, Ada.Characters.Latin_1.LF);
                                    Ada.Strings.Unbounded.Append (Result, Indentation.Value);
                                    Pos := Indentation.Length;
                                 end if;
                              end if;
                           end Process_Mode_Break;

                           -----------------------
                           -- Process_Mode_Flat --
                           -----------------------

                           procedure Process_Mode_Flat is
                           begin
                              if not Document.Bare_Document.Command.Hard then
                                 if not Document.Bare_Document.Command.Soft then
                                    Ada.Strings.Unbounded.Append (Result, " ");
                                    Pos := @ + 1;
                                 end if;

                              else
                                 Should_Remeasure := True;
                                 Process_Mode_Break;
                              end if;
                           end Process_Mode_Flat;

                        begin
                           case Mode is
                              when Mode_Flat =>
                                 Process_Mode_Flat;

                              when Mode_Break =>
                                 Process_Mode_Break;
                           end case;
                        end;

                     --  when Command_Hard_Line =>
                     --     raise Program_Error;

                     --  when Command_Soft_Line =>
                     --     raise Program_Error;

                     --  when Command_Literal_Line =>
                     --     raise Program_Error;

                     when Command_Label =>
                        Print_Commands.Append
                          (Print_Command_Type'
                           (Indentation, Mode, Document.Bare_Document.Command.Label_Contents));

                     when Command_Break_Parent =>
                        null;
                  end case;

            end case;
         end;
      end loop;

      Gnatfmt.Documents.Builders.Reset_Document_Id;

      return Ada.Strings.Unbounded.To_String (Result);

   exception
      when others =>
         Gnatfmt.Documents.Builders.Reset_Document_Id;
         return "";
   end Format;

   --------------------------
   -- Generate_Indentation --
   --------------------------

   function Generate_Indentation
     (From    : Indentation_Type;
      Data    : Indentation_Data_Type;
      Options : Indentation_Options_Type)
      return Indentation_Type
   is
      Value  : Ada.Strings.Unbounded.Unbounded_String;
      Length : Natural := 0;
      Queue  : Indentation_Data_Vector := From.Queue;

      Last_Tabs   : Natural := 0;
      Last_Spaces : Natural := 0;

      procedure Add_Tabs (Count : Natural);
      --  TODO: Description

      procedure Add_Spaces (Count : Natural);
      --  TODO: Description

      procedure Flush;
      --  TODO: Description

      procedure Flush_Tabs;
      --  TODO: Description

      procedure Flush_Spaces;
      --  TODO: Description

      procedure Reset_Last;
      --  TODO: Description

      --------------
      -- Add_Tabs --
      --------------

      procedure Add_Tabs (Count : Natural)
      is
      begin
         Ada.Strings.Unbounded.Append
           (Value, Ada.Strings.Unbounded."*" (Count, Ada.Characters.Latin_1.HT));
         Length := @ + Options.Width * Count;
      end Add_Tabs;

      ----------------
      -- Add_Spaces --
      ----------------

      procedure Add_Spaces (Count : Natural)
      is
      begin
         Ada.Strings.Unbounded.Append
           (Value, Ada.Strings.Unbounded."*" (Count, Ada.Characters.Latin_1.Space));
         Length := @ + Count;
      end Add_Spaces;

      -----------
      -- Flush --
      -----------

      procedure Flush
      is
      begin
         case Options.Kind is
            when Tabs => Flush_Tabs;
            when Spaces => Flush_Spaces;
         end case;
      end Flush;

      ----------------
      -- Flush_Tabs --
      ----------------

      procedure Flush_Tabs
      is
      begin
         if Last_Tabs > 0 then
            Add_Tabs (Last_Tabs);
         end if;
         Reset_Last;
      end Flush_Tabs;

      ------------------
      -- Flush_Spaces --
      ------------------

      procedure Flush_Spaces
      is
      begin
         if Last_Spaces > 0 then
            Add_Spaces (Last_Spaces);
         end if;
         Reset_Last;
      end Flush_Spaces;

      ----------------
      -- Reset_Last --
      ----------------

      procedure Reset_Last
      is
      begin
         Last_Tabs := 0;
         Last_Spaces := 0;
      end Reset_Last;

   begin
      if Data.Kind = Dedent then
         Queue.Delete_Last;

      else
         Queue.Append (Data);
      end if;

      for Part of Queue loop
         case Part.Kind is
            when Indent =>
               Flush;
               case Options.Kind is
                  when Tabs => Add_Tabs (1);
                  when Spaces => Add_Spaces (Options.Width);
               end case;

            when String_Align =>
               Flush;
               Ada.Strings.Unbounded.Append (Value, Data.T);
               Length := @ + Ada.Strings.Unbounded.Length (Data.T);

            when Number_Align =>
               Last_Tabs := @ + 1;
               Last_Spaces := @ + Data.N;

            when Dedent =>
               raise Program_Error; -- TODO: Make this a logic error
         end case;
      end loop;

      Flush_Spaces;

      return Indentation_Type'(Value, Length, Queue, From.Root);
   end Generate_Indentation;

   ------------------------
   -- Get_Document_Parts --
   ------------------------

   function Get_Document_Parts
     (Document : Document_Type)
      return Document_Type_Array_Access
   is ((case Document.Bare_Document.Kind is
           when Document_List => Document.Bare_Document.List,
           when Document_Command =>
              (case Document.Bare_Document.Command.Kind is
                  when Command_Fill =>
                     Document.Bare_Document.Command.Parts.Bare_Document.List,
                  when others => raise Program_Error),
           when others => raise Program_Error));
   --  TODO: Replace Program_Error by a Gnatfmt defined exception

   ----------------------
   -- Make_Indentation --
   ----------------------

   function Make_Indentation
     (From    : Indentation_Type;
      Options : Indentation_Options_Type)
      return Indentation_Type
   is (Generate_Indentation
         (From, Indentation_Data_Type'(Kind => Indent), Options));

   ----------------
   -- Make_Align --
   ----------------

   function Make_Align
     (From       : Indentation_Type;
      Align_Data : Align_Data_Type;
      Options    : Format_Options_Type)
      return Indentation_Type
   is

   begin
      case Align_Data.Kind is
         when None =>
            return From;

         when Width =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'(String_Align, Align_Data.T),
                 Options.Indentation);

         when Text =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'(Number_Align, Align_Data.N),
                 Options.Indentation);

         when To_Root =>
            if From.Root /= null then
               return From.Root.all;

            else
               return Root_Indent;
            end if;

         when Dedent =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'(Kind => Dedent),
                 Options.Indentation);

         when Root =>
            declare
               Result : Indentation_Type := From;

            begin
               Result.Root := new Indentation_Type'(From);
               return Result;
            end;
      end case;

   end Make_Align;

   function Last
     (Document_List : Document_Type_Array_Access)
      return Document_Type
   is (Document_List (Document_List'Last));

   ----------------------
   -- Propagate_Breaks --
   ----------------------

   procedure Propagate_Breaks (Document : Document_Type) is
      Already_Visited : Document_Type_Hashed_Set;
      Group_Stack     : Group_Command_Type_Vector;

      function Propagate_Breaks_On_Enter
        (Document : Document_Type)
         return Optional_Boolean;
      --  TODO

      function Propagate_Breaks_On_Exit
        (Document : Document_Type)
         return Optional_Boolean;
      --  TODO

      -------------------------------
      -- Propagate_Breaks_On_Enter --
      -------------------------------

      function Propagate_Breaks_On_Enter
        (Document : Document_Type)
         return Optional_Boolean
      is

      begin
         case Document.Bare_Document.Kind is
            when Document_Command =>
               case Document.Bare_Document.Command.Kind is
                  when Command_Break_Parent =>
                     Break_Parent_Group (Group_Stack);

                  when Command_Group =>
                     Group_Stack.Append (Document.Bare_Document.Command);
                     if Already_Visited.Contains (Document) then
                        return
                          Optional_Boolean'
                            (Is_Set => True, Value => False);
                     else
                        Already_Visited.Insert (Document);
                     end if;

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;

         return Optional_Boolean'(Is_Set => False);
      end Propagate_Breaks_On_Enter;

      ------------------------------
      -- Propagate_Breaks_On_Exit --
      ------------------------------

      function Propagate_Breaks_On_Exit
        (Document : Document_Type)
         return Optional_Boolean
      is
      begin
         case Document.Bare_Document.Kind is
            when Document_Command =>
               if Document.Bare_Document.Command.Kind in Command_Group then
                  declare
                     Group : constant Command_Type_Access :=
                       Group_Stack.Last_Element;

                  begin
                     Group_Stack.Delete_Last;
                     if Group.Break then
                        Break_Parent_Group (Group_Stack);
                     end if;
                  end;
               end if;

            when others =>
               null;
         end case;

         return Optional_Boolean'(Is_Set => False);
      end Propagate_Breaks_On_Exit;

   begin
      Traverse_Document
        (Document                           => Document,
         On_Enter                           => Propagate_Breaks_On_Exit'Access,
         On_Exit                            =>
           Propagate_Breaks_On_Enter'Access,
         Should_Traverse_Conditional_Groups => True);
   end Propagate_Breaks;

   -----------------
   -- Root_Indent --
   -----------------

   function Root_Indent return Indentation_Type is
      (Indentation_Type'
         (Value  => Ada.Strings.Unbounded.Null_Unbounded_String,
         Length => 0,
         Queue  => [],
         Root   => null));

   -----------------------
   -- Traverse_Document --
   -----------------------

   procedure Traverse_Document
     (Document                           : Document_Type;
      On_Enter                           :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      On_Exit                            :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      Should_Traverse_Conditional_Groups : Boolean := False)
   is
      use type Ada.Containers.Count_Type;

      Traverse_Doc_On_Exit_Stack_Marker : constant Document_Type :=
        Document_Type'(Bare_Document => null);

      Doc_Stack : Document_Type_Vector := [Document];

   begin
      while Doc_Stack.Length > 0 loop
         <<Continue>>
         declare
            Doc : Document_Type :=
              Doc_Stack.Last_Element;

         begin
            Doc_Stack.Delete_Last;

            if Doc = Traverse_Doc_On_Exit_Stack_Marker then
               Doc := Doc_Stack.Last_Element;
               Doc_Stack.Delete_Last;
               declare
                  Ignore : constant Optional_Boolean :=
                    On_Exit (Doc);
               begin
                  goto Continue;
               end;
            end if;

            if On_Exit /= null then
               Doc_Stack.Append (Traverse_Doc_On_Exit_Stack_Marker);
            end if;

            if On_Enter /= null then
               declare
                  On_Enter_Result : constant Optional_Boolean :=
                    On_Enter (Doc);
               begin
                  if On_Enter_Result.Is_Set
                    and then not On_Enter_Result.Value
                  then
                     goto Continue;
                  end if;
               end;
            end if;

            case Doc.Bare_Document.Kind is
               when Document_Text =>
                  null;

               when Document_List =>
                  for Child_Doc of reverse Doc.Bare_Document.List.all loop
                     Doc_Stack.Append (Child_Doc);
                  end loop;

               when Document_Command =>
                  case Doc.Bare_Document.Command.Kind is
                     when Command_Align =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Align_Contents);

                     when Command_Indent =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Indent_Contents);

                     when Command_Indent_If_Break =>
                        Doc_Stack.Append
                          (Doc
                             .Bare_Document
                             .Command
                             .Indent_If_Break_Contents);

                     when Command_Label =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Label_Contents);

                     when Command_Line_Suffix =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Line_Suffix_Contents);

                     when Command_Fill =>
                        for Child_Doc of reverse
                          --  TODO: How to make this safe? Is Parts always a
                          --  list?
                          Doc
                            .Bare_Document
                            .Command
                            .Parts
                            .Bare_Document
                            .List
                            .all
                        loop
                           Doc_Stack.Append (Child_Doc);
                        end loop;

                     when Command_Group =>
                        if Should_Traverse_Conditional_Groups then
                           for Child_Doc of reverse
                             --  TODO: How to make this safe? Is
                             --  Expanded_States always a list?
                             Doc
                               .Bare_Document
                               .Command
                               .Expanded_States
                               .Bare_Document
                               .List
                               .all
                           loop
                              Doc_Stack.Append (Child_Doc);
                           end loop;

                        else
                           Doc_Stack.Append
                             (Doc.Bare_Document.Command.Group_Contents);
                        end if;

                     when Command_If_Break =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Flat_Contents);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Break_Contents);

                     when Command_Break_Parent
                          | Command_Cursor
                          | Command_Line
                        --    | Command_Soft_Line
                        --    | Command_Hard_Line
                        --    | Command_Literal_Line
                          | Command_Line_Suffix_Boundary
                          | Command_Trim =>
                        null;

                  end case;

            end case;
         end;
      end loop;
   end Traverse_Document;

   ----------
   -- Trim --
   ----------

   function Trim
     (Text : in out Ada.Strings.Unbounded.Unbounded_String)
      return Natural
   is
      use Ada.Strings.Unbounded;

      Initial_Length : constant Natural := Length (Text);

   begin
      Trim (Text, Ada.Strings.Right);

      return Initial_Length - Length (Text);
   end Trim;

end Gnatfmt.Documents;
