--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings;

with VSS.Characters;
with VSS.Characters.Latin;
with VSS.Strings.Conversions;
with VSS.String_Vectors;
with VSS.Text_Streams;
with VSS.Text_Streams.File_Input;
with VSS.Text_Streams.File_Output;
with VSS.Strings.Cursors.Iterators;
with VSS.Strings.Cursors.Iterators.Characters;

package body Gnatformat.Edits is
   package Positive_Vectors is new Ada.Containers.Vectors (Positive, Positive);

   subtype Positive_Vector is Positive_Vectors.Vector;

   use type VSS.Strings.Virtual_String;

   package File_Name_To_Virtual_String_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Source_Path_Type,
        Element_Type => VSS.Strings.Virtual_String);

   subtype File_Name_To_Virtual_String_Map is
     File_Name_To_Virtual_String_Maps.Map;

   function Apply_Edits
     (Edits : Formatting_Edits_Type)
      return File_Name_To_Virtual_String_Map;
   --  Apply Edits on memory

   procedure Compute_Line_Length
     (Line        : VSS.Strings.Virtual_String;
      Length      : out Natural;
      Tabs_Length : out Positive_Vector);
   --  Computes Line's Length by iterating through each character and
   --  considering that each VSS.Characters.Latin.Character_Tabulation has a
   --  maximum length of 8. The length of each
   --  VSS.Characters.Latin.Character_Tabulation also appended to Tabs_Length.

   -----------------
   -- Apply_Edits --
   -----------------

   procedure Apply_Edits (Edits : Formatting_Edits_Type)
   is
      use File_Name_To_Virtual_String_Maps;

      File_Edits   : constant File_Name_To_Virtual_String_Map :=
        Apply_Edits (Edits);
      Edits_Cursor : Cursor := File_Edits.First;

   begin
      if File_Edits.Is_Empty then
         return;
      end if;

      while Has_Element (Edits_Cursor) loop
         declare
            Output : VSS.Text_Streams.File_Output.File_Output_Text_Stream;
            Buffer : constant VSS.Strings.Virtual_String :=
              Element (Edits_Cursor);
            Ignore : Boolean := True;

         begin
            Output.Create
              (VSS.Strings.Conversions.To_Virtual_String (Key (Edits_Cursor)));
            Output.Put (Buffer, Ignore);
            Output.Close;

         exception
            when E : others =>
               Gnatformat_Trace.Trace (E);
         end;

         Next (Edits_Cursor);
      end loop;
   end Apply_Edits;

   -----------------
   -- Apply_Edits --
   -----------------

   function Apply_Edits
     (Edits : Formatting_Edits_Type)
      return File_Name_To_Virtual_String_Map
   is
      use Formatting_Edit_Hashed_Maps;

      Edits_Cursor : Formatting_Edit_Hashed_Maps.Cursor := Edits.First;
      Result       : File_Name_To_Virtual_String_Map;

   begin
      if Edits.Is_Empty then
         return Result;
      end if;

      while Has_Element (Edits_Cursor) loop
         declare
            Original_Filename : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String (Key (Edits_Cursor));
            Original_File     :
              VSS.Text_Streams.File_Input.File_Input_Text_Stream;
            Input_Buffer      : VSS.Strings.Virtual_String;
            Output_Buffer     : VSS.Strings.Virtual_String;

            Text_Edits        : constant Constant_Reference_Type :=
              Constant_Reference (Edits, Edits_Cursor);
            Text_Edits_Cursor : Text_Edit_Ordered_Sets.Cursor :=
              Text_Edits.Last;
            Current_Text_Edit : Text_Edit_Type :=
              Text_Edit_Ordered_Sets.Element (Text_Edits_Cursor);
            Inside_Text_Edit  : Boolean := False;

            Current_Line_Number   : Natural;
            Current_Column_Number : Natural;

            Current_Character : VSS.Characters.Virtual_Character;
            Success           : Boolean := True;

         begin
            Original_File.Open (Original_Filename, "utf-8");
            while not Original_File.Is_End_Of_Stream loop
               Original_File.Get (Current_Character, Success);
               Input_Buffer.Append (Current_Character);
            end loop;
            Original_File.Close;

            declare
               Lines          :
                 constant VSS.String_Vectors.Virtual_String_Vector :=
                   Input_Buffer.Split_Lines (Keep_Terminator => True);
               Lines_Iterator :
                 constant VSS.String_Vectors.Reversible_Iterator :=
                   VSS.String_Vectors.Iterate (Lines);
               Lines_Cursor   : VSS.String_Vectors.Cursor :=
                 VSS.String_Vectors.Last (Lines_Iterator);

               Current_Line           : VSS.Strings.Virtual_String;
               Current_Line_Length    : Natural;
               Current_Line_Tabs_Size : Positive_Vector;

               Ignore : Boolean;

               use type VSS.Characters.Virtual_Character;

            begin
               Current_Line_Tabs_Size.Reserve_Capacity (10);

               Current_Line_Number := Lines.Length;
               while VSS.String_Vectors.Has_Element (Lines_Cursor) loop
                  Current_Line :=
                    VSS.String_Vectors.Element (Lines, Lines_Cursor);

                  Compute_Line_Length
                    (Current_Line,
                     Current_Line_Length,
                     Current_Line_Tabs_Size);

                  Current_Column_Number := Current_Line_Length;

                  declare
                     Current_Line_Character_It :
                       VSS.Strings.Cursors.Iterators.Characters.
                         Character_Iterator :=
                           Current_Line.After_Last_Character;

                  begin
                     while Current_Line_Character_It.Backward loop
                        if not Inside_Text_Edit then
                           Output_Buffer.Prepend
                             (Current_Line_Character_It.Element);
                        end if;

                        if Current_Text_Edit /= No_Text_Edit then
                           if Current_Line_Number =
                                Natural (Current_Text_Edit.Location.Start_Line)
                             and then Current_Column_Number =
                                        Natural
                                          (Current_Text_Edit.Location.
                                             Start_Column)
                           then
                              Text_Edit_Ordered_Sets.Previous
                                (Text_Edits_Cursor);
                              if Text_Edit_Ordered_Sets.
                                   Has_Element (Text_Edits_Cursor)
                              then
                                 Current_Text_Edit :=
                                   Text_Edit_Ordered_Sets.
                                     Element (Text_Edits_Cursor);
                              else
                                 Current_Text_Edit := No_Text_Edit;
                              end if;
                              Inside_Text_Edit := False;
                           end if;

                           if Current_Line_Number =
                                Natural (Current_Text_Edit.Location.End_Line)
                             and then Current_Column_Number =
                                        Natural
                                          (Current_Text_Edit.Location.
                                             End_Column)
                           then
                              Output_Buffer.Prepend
                                (VSS.Strings.Conversions.To_Virtual_String
                                   (Ada.Strings.Unbounded.To_String
                                      (Current_Text_Edit.Text)));

                              Inside_Text_Edit := True;
                           end if;
                        end if;

                        if Current_Line_Character_It.Element =
                          VSS.Characters.Latin.Character_Tabulation
                        then
                           Current_Column_Number :=
                             @ - Current_Line_Tabs_Size.Last_Element;
                           Current_Line_Tabs_Size.Delete_Last;
                        else
                           Current_Column_Number := @ - 1;
                        end if;
                     end loop;
                  end;

                  Lines_Cursor :=
                    VSS.String_Vectors.Previous (Lines_Iterator, Lines_Cursor);
                  Current_Line_Number := @ - 1;
               end loop;
            end;

            Result.Insert
              (Formatting_Edit_Hashed_Maps.Key (Edits_Cursor),
               Output_Buffer);

         exception
            when E : others =>
               Gnatformat_Trace.Trace (E);
         end;

         Formatting_Edit_Hashed_Maps.Next (Edits_Cursor);
      end loop;

      return Result;
   end Apply_Edits;

   -------------------------
   -- Compute_Line_Length --
   -------------------------

   procedure Compute_Line_Length
     (Line        : VSS.Strings.Virtual_String;
      Length      : out Natural;
      Tabs_Length : out Positive_Vector)
   is
      Character_It :
        VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
          Line.Before_First_Character;

      use type VSS.Characters.Virtual_Character;

   begin
      Length := 0;
      Tabs_Length := Positive_Vectors.Empty_Vector;
      while Character_It.Forward loop
         if Character_It.Element =
              VSS.Characters.Latin.Character_Tabulation
         then
            Tabs_Length.Append (8 - (Length mod 8));
            Length := @ + (8 - (@ mod 8));
         else
            Length := @ + 1;
         end if;
      end loop;
   end Compute_Line_Length;

   -----------
   -- Image --
   -----------

   function Image (Edit : Gnatformat.Edits.Formatting_Edit_Type) return String
   is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Langkit_Support.Slocs;

   begin
      return
        Simple_Name (Edit.Unit.Get_Filename)
        & "("
        & Edit.Formatted_Node.Image
        & ") - "
        & Image (Edit.Text_Edit.Location)
        & Ada.Characters.Latin_1.LF
        & '^'
        & Ada.Characters.Latin_1.LF
        & To_String (Edit.Text_Edit.Text)
        & '$';
   end Image;

   ----------------------
   -- Insert_Text_Edit --
   ----------------------

   procedure Insert_Text_Edit
     (Map         : in out Formatting_Edit_Hashed_Maps.Map;
      Source_Path : Source_Path_Type;
      Text_Edit   : Text_Edit_Type)
   is
   begin
      if Map.Contains (Source_Path) then
         Map.Reference (Source_Path).Insert (Text_Edit);

      else
         declare
            Edits : Text_Edit_Ordered_Set;

         begin
            Edits.Insert (Text_Edit);
            Map.Insert (Source_Path, Edits);
         end;
      end if;
   end Insert_Text_Edit;

end Gnatformat.Edits;
