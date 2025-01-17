--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories;

with Libadalang.Generic_API;
with Langkit_Support.Text;
with Libadalang.Common;

with Prettier_Ada.Documents; use Prettier_Ada.Documents;

with Gnatformat.Helpers;

package body Gnatformat.Formatting is

   function Restore_Off_On_Sections
     (Original_Source  : Ada.Strings.Unbounded.Unbounded_String;
      Formatted_Source : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Restores sections of the formatted source code that are delimitted by
   --  the off/on markers by copying them from the original source code.
   --
   --  The current markers are:
   --  '--!format off' and '--!format on'
   --  '--!pp off' and '--!pp on'
   --  '--  begin read only' and '--  end read only'

   ------------
   -- Format --
   ------------

   function Format
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String
   is (Format
         (Libadalang.Generic_API.To_Generic_Unit (Unit),
          Format_Options.Into
            (Ada.Directories.Simple_Name (Unit.Get_Filename), Ada_Language),
          Configuration));

   -----------------------------
   -- Restore_Off_On_Sections --
   -----------------------------

   function Restore_Off_On_Sections
     (Original_Source  : Ada.Strings.Unbounded.Unbounded_String;
      Formatted_Source : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use type Ada.Containers.Count_Type;

      function "+"
        (Source : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

      type Marker_Kind is (Off, On);
      --  A marker can indicate either the start or the end of an On / Off
      --  section.

      function "<" (Left, Right : Marker_Kind) return Boolean
      is (if Left = Right then False else Left = Off and Right = On);
      --  The Off marker is always expected before the On marker

      type On_Off_Section_Marker is
        array (Marker_Kind) of Ada.Strings.Unbounded.Unbounded_String;

      type On_Off_Section_Marker_Index is new Positive;

      type On_Off_Section_Marker_Array is
        array (On_Off_Section_Marker_Index range <>) of On_Off_Section_Marker;

      On_Off_Section_Markers : constant On_Off_Section_Marker_Array :=
        [[+"--!format off", +"--!format on"],
         [+"--!pp off", +"--!pp on"],
         [+"--  begin read only", +"--  end read only"]];

      type Marker_Information_Record is record
         Index_On_String : Positive;
         --  The character (byte) index of the String where this marker starts

         Marker_Index    : On_Off_Section_Marker_Index;
         --  The index (identifier) of this marker on the markers list
         --  On_Off_Section_Markers.

         Kind            : Marker_Kind;
      end record;

      function "<" (Left, Right : Marker_Information_Record) return Boolean
      is (case Left.Index_On_String = Right.Index_On_String is
            when True =>
              (case Left.Marker_Index = Right.Marker_Index is
                 when True => Left.Kind < Right.Kind,
                 when False => Left.Marker_Index < Right.Marker_Index),
            when False => Left.Index_On_String < Right.Index_On_String);
      --  Markers are sorted on the following order:
      --  1) The character (byte) index of the String where the marker starts
      --  2) Their kind (Off marker is expected before than On marker)
      --  3) The index (identifier) of this marker on the markers list
      --     On_Off_Section_Markers.

      package Marker_Information_Vectors is new
        Ada.Containers.Vectors (Positive, Marker_Information_Record);
      subtype Marker_Information_Vector is Marker_Information_Vectors.Vector;
      package Marker_Info_Vector_Sorting is new
        Marker_Information_Vectors.Generic_Sorting ("<");

      function Find_Markers_Information
        (Source : Ada.Strings.Unbounded.Unbounded_String)
         return Marker_Information_Vector;
      --  Finds all markers in Source and computed their
      --  Marker_Information_Record. Returns a Marker_Information_Vector with
      --  all the found markers.
      --
      --  This function assumes that On_Off_Section_Markers is composed by
      --  ISO 8859-1 (Latin-1) characters.

      function Restore_Off_On_Sections
        (Original_Source          : Ada.Strings.Unbounded.Unbounded_String;
         Original_Source_Markers  : Marker_Information_Vector;
         Formatted_Source         : Ada.Strings.Unbounded.Unbounded_String;
         Formatted_Source_Markers : Marker_Information_Vector)
         return Ada.Strings.Unbounded.Unbounded_String;
      --  Restores sections of the Formatted_Source code that are delimitted by
      --  Formatted_Source_Markers, by copying them from Original_Source.
      --  Original_Source_Markers are the markers equivalent to
      --  Formatted_Source_Markers but in the Original_Source.
      --
      --  This function assumes that On_Off_Section_Markers is composed by
      --  ISO 8859-1 (Latin-1) characters.

      procedure Validate_Markers
        (Markers_Information : Marker_Information_Vector);
      --  Validates Markers_Information by checking that:
      --  Groups of two markers are of the same kind, for example,
      --  '--!format off' and '--!format on'.
      --  The last marker is allowed to not be in a group of two, as long as it
      --  is an off marker.
      --
      --  Throws Internal_Error_Off_On_Invalid_Marker or Off_On_Invalid_Marker
      --  if Markers_Information is not in a good state.

      procedure Validate_Markers
        (Left : Marker_Information_Vector; Right : Marker_Information_Vector);
      --  Validades Left and Right by checking that they match each other.
      --  Two Marker_Information_Vector match if their zip result in a tuple of
      --  equivalent markers.
      --
      --  Throws Internal_Error_Off_On_Invalid_Marker if Left and Right are
      --  not equivalent.

      ------------------------------
      -- Find_Markers_Information --
      ------------------------------

      function Find_Markers_Information
        (Source : Ada.Strings.Unbounded.Unbounded_String)
         return Marker_Information_Vector
      is
         function Is_Whole_Line
           (Marker_Start, Marker_End : Positive) return Boolean;
         --  Checks if the marker delimited by Marker_Start and Marker_End is
         --  in a whole line, i.e., if after skipping leading and trailing
         --  spaces, the marker is preceded and succeded by LF or the CR LF
         --  sequence.

         -------------------
         -- Is_Whole_Line --
         -------------------

         function Is_Whole_Line
           (Marker_Start, Marker_End : Positive) return Boolean
         is
            Source_Length : constant Natural :=
              Ada.Strings.Unbounded.Length (Source);

            Next_Must_Be_LF : Boolean := False;

         begin
            --  Check that after skipping leading spaces, we find LF.

            if Marker_Start /= 1 then
               for J in reverse 1 .. Marker_Start - 1 loop
                  if Ada.Strings.Unbounded.Element (Source, J)
                    = Ada.Characters.Latin_1.Space
                  then
                     null;
                  elsif Ada.Strings.Unbounded.Element (Source, J)
                    = Ada.Characters.Latin_1.LF
                  then
                     exit;
                  else
                     return False;
                  end if;
               end loop;
            end if;

            --  Check that after skipping trailing spaces, we find LF or the
            --  CR LF sequence.

            if Marker_End /= Source_Length then
               for J in Marker_End + 1 .. Source_Length loop
                  if Ada.Strings.Unbounded.Element (Source, J) = ' ' then
                     if Next_Must_Be_LF then
                        return False;
                     end if;

                  elsif Ada.Strings.Unbounded.Element (Source, J)
                    = Ada.Characters.Latin_1.LF
                  then
                     exit;

                  elsif Ada.Strings.Unbounded.Element (Source, J)
                    = Ada.Characters.Latin_1.CR
                  then
                     if Next_Must_Be_LF then
                        return False;
                     end if;
                     Next_Must_Be_LF := True;

                  else
                     return False;
                  end if;
               end loop;
            end if;

            return True;
         end Is_Whole_Line;

         Markers_Information : Marker_Information_Vector := [];

      begin
         for On_Off_Section_Marker_Index in On_Off_Section_Markers'Range loop
            declare
               From : Natural := 1;
               --  The index from where the string search starts

               Current_Marker : Marker_Kind := Off;
               --  The current marker we're looking. Starts with Off. Any On
               --  markers that come before the first Off marker are simply
               --  ignored.

            begin
               while From < Ada.Strings.Unbounded.Length (Source) loop
                  From :=
                    Ada.Strings.Unbounded.Index
                      (Source,
                       Ada.Strings.Unbounded.To_String
                         (On_Off_Section_Markers (On_Off_Section_Marker_Index)
                            (Current_Marker)),
                       From);

                  --  If the marker was not found, exit

                  if From = 0 then
                     exit;
                  end if;

                  --  Only consider this marker if it's a whole line comment

                  if Is_Whole_Line
                       (From,
                        From
                        + Ada.Strings.Unbounded.Length
                            (On_Off_Section_Markers
                               (On_Off_Section_Marker_Index)
                                  (Current_Marker))
                        - 1)
                  then
                     Markers_Information.Append
                       (Marker_Information_Record'
                          (Positive (From),
                           On_Off_Section_Marker_Index,
                           Current_Marker));

                     --  Flip the marker we're looking for

                     Current_Marker :=
                       (case Current_Marker is
                          when On => Off,
                          when Off => On);
                  end if;

                  --  Next search starts after the marker that was just found

                  From :=
                    @
                    + Ada.Strings.Unbounded.Length
                        (On_Off_Section_Markers (On_Off_Section_Marker_Index)
                           (Current_Marker));
               end loop;
            end;

         end loop;

         Marker_Info_Vector_Sorting.Sort (Markers_Information);

         return Markers_Information;
      end Find_Markers_Information;

      -----------------------------
      -- Restore_Off_On_Sections --
      -----------------------------

      function Restore_Off_On_Sections
        (Original_Source          : Ada.Strings.Unbounded.Unbounded_String;
         Original_Source_Markers  : Marker_Information_Vector;
         Formatted_Source         : Ada.Strings.Unbounded.Unbounded_String;
         Formatted_Source_Markers : Marker_Information_Vector)
         return Ada.Strings.Unbounded.Unbounded_String
      is
         Marker_Index  : Positive := 1;
         Markers_Count : constant Positive :=
           Positive (Original_Source_Markers.Length);

         Result : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Null_Unbounded_String;

      begin
         --  Copy the formatted section before the first Off marker

         Ada.Strings.Unbounded.Append
           (Result,
            Ada.Strings.Unbounded.Slice
              (Formatted_Source,
               1,
               Formatted_Source_Markers (Marker_Index).Index_On_String - 1));

         while Marker_Index <= Markers_Count loop
            if Marker_Index + 1 > Markers_Count then
               --  This is the last marker, and if an Off marker

               --  Restore until the end of the source

               Ada.Strings.Unbounded.Append
                 (Result,
                  Ada.Strings.Unbounded.Slice
                    (Original_Source,
                     Original_Source_Markers (Marker_Index).Index_On_String,
                     Ada.Strings.Unbounded.Length (Original_Source)));

            else
               --  This is not the last marker, so it's an off marker with a
               --  corresponding on marker.

               --  Restore the Off section up until, but not including, the On
               --  marker.

               Ada.Strings.Unbounded.Append
                 (Result,
                  Ada.Strings.Unbounded.Slice
                    (Original_Source,
                     Original_Source_Markers (Marker_Index).Index_On_String,
                     Original_Source_Markers (Marker_Index + 1).Index_On_String
                     - 1));

               if Marker_Index + 2 > Markers_Count then
                  --  'Marker_Index + 1' is the last marker and it is an On
                  --  marker. Copy the rest of the formatted source, including
                  --  the marker.

                  Ada.Strings.Unbounded.Append
                    (Result,
                     Ada.Strings.Unbounded.Slice
                       (Formatted_Source,
                        Formatted_Source_Markers (Marker_Index + 1)
                          .Index_On_String,
                        Ada.Strings.Unbounded.Length (Formatted_Source)));

               else
                  --  There is at least one more Off marker. Copy from this On
                  --  marker until, but not including, the next Off marker.

                  Ada.Strings.Unbounded.Append
                    (Result,
                     Ada.Strings.Unbounded.Slice
                       (Formatted_Source,
                        Formatted_Source_Markers (Marker_Index + 1)
                          .Index_On_String,
                        Formatted_Source_Markers (Marker_Index + 2)
                          .Index_On_String
                        - 1));
               end if;
            end if;

            Marker_Index := @ + 2;
         end loop;

         return Result;
      end Restore_Off_On_Sections;

      ----------------------
      -- Validate_Markers --
      ----------------------

      procedure Validate_Markers
        (Markers_Information : Marker_Information_Vector) is
      begin
         if Markers_Information.Is_Empty then
            return;
         end if;

         declare
            Marker_Index  : Positive := 1;
            Markers_Count : constant Positive :=
              Positive (Markers_Information.Length);

         begin
            while Marker_Index <= Markers_Count loop
               if Markers_Information.Constant_Reference (Marker_Index).Kind
                 /= Off
               then
                  raise Internal_Error_Off_On_Invalid_Marker
                    with
                      f"On / Off marker section mismatch, expected an off "
                      & "marker, found "
                      & f"{On_Off_Section_Markers
                             (Markers_Information.Constant_Reference
                                (Marker_Index)
                                .Marker_Index)
                                (On)}";
               end if;

               if Marker_Index + 1 > Markers_Count then
                  exit;
               end if;

               if Markers_Information.Constant_Reference (Marker_Index + 1)
                    .Kind
                 /= On
               then
                  raise Off_On_Invalid_Marker
                    with
                      f"On / Off marker section mismatch, expected "
                      & f"{On_Off_Section_Markers
                              (Markers_Information.Constant_Reference
                                 (Marker_Index)
                                 .Marker_Index)
                                 (On)}"
                      & ", found "
                      & f"{On_Off_Section_Markers
                              (Markers_Information.Constant_Reference
                                 (Marker_Index + 1)
                                 .Marker_Index)
                                 (Off)}";
               end if;

               if Markers_Information.Constant_Reference (Marker_Index)
                    .Marker_Index
                 /= Markers_Information.Constant_Reference (Marker_Index + 1)
                      .Marker_Index
               then
                  raise Internal_Error_Off_On_Invalid_Marker
                    with
                      f"Invalid On / Off section, marker "
                      & f"{On_Off_Section_Markers
                              (Markers_Information.Constant_Reference
                                 (Marker_Index)
                                 .Marker_Index)
                                 (Off)}"
                      & ", followed by "
                      & f"{On_Off_Section_Markers
                              (Markers_Information.Constant_Reference
                                 (Marker_Index + 1)
                                 .Marker_Index)
                                 (On)}";
               end if;

               Marker_Index := @ + 2;
            end loop;
         end;
      end Validate_Markers;

      ----------------------
      -- Validate_Markers --
      ----------------------

      procedure Validate_Markers
        (Left : Marker_Information_Vector; Right : Marker_Information_Vector)
      is
      begin
         if Left.Length /= Right.Length then
            raise Internal_Error_Off_On_Invalid_Marker
              with
                "Original source does not contain the same amount of markers "
                & "as the formatted source";
         end if;

         if Left.Is_Empty then
            return;
         end if;

         declare
            Marker_Index  : Positive := 1;
            Markers_Count : constant Positive := Positive (Left.Length);

         begin
            while Marker_Index <= Markers_Count loop
               if Left.Constant_Reference (Marker_Index).Marker_Index
                 /= Right.Constant_Reference (Marker_Index).Marker_Index
               then
                  raise Internal_Error_Off_On_Invalid_Marker
                    with
                      "Original source does not contain the same markers "
                      & "sequence as the formatted source";
               end if;

               Marker_Index := @ + 1;
            end loop;
         end;
      end Validate_Markers;

   begin
      --  Start by finding the Off / On markers in the original source

      Original_Source_Markers : constant Marker_Information_Vector :=
        Find_Markers_Information (Original_Source);

      --  If no markers are found in the original source, then there's nothing
      --  to restore. Return the formatted source.

      if Original_Source_Markers.Length = 0 then
         return Formatted_Source;
      end if;

      --  Validate the the markers found in the original source are in a good
      --  state.

      Validate_Markers (Original_Source_Markers);

      --  Find the Off / On markers in the formatted source

      Formatted_Source_Markers : constant Marker_Information_Vector :=
        Find_Markers_Information (Formatted_Source);

      --  Validate the the markers found in the formatted source are in a good
      --  state.

      Validate_Markers (Formatted_Source_Markers);

      --  Validate the markers found in the original and formatted sources
      --  match each other.

      Validate_Markers (Original_Source_Markers, Formatted_Source_Markers);

      --  Finally restore the Off / On regions based on the markers

      return
        Restore_Off_On_Sections
          (Original_Source,
           Original_Source_Markers,
           Formatted_Source,
           Formatted_Source_Markers);
   end Restore_Off_On_Sections;

   ------------
   -- Format --
   ------------

   function Format
     (Unit           : Langkit_Support.Generic_API.Analysis.Lk_Unit;
      Format_Options : Prettier_Ada.Documents.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Document : constant Prettier_Ada.Documents.Document_Type :=
        Langkit_Support.Generic_API.Unparsing.Unparse_To_Prettier
          (Unit.Root, Configuration);

      Original_Source  : constant Ada.Strings.Unbounded.Unbounded_String :=
        Gnatformat.Helpers.Read_To_Unbounded_String (Unit.Filename);
      Formatted_Source : constant Ada.Strings.Unbounded.Unbounded_String :=
        Prettier_Ada.Documents.Format (Document, Format_Options);

   begin
      return Restore_Off_On_Sections (Original_Source, Formatted_Source);
   end Format;

   function Format_Unit
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      --  Unparsing_Config_File : GNATCOLL.VFS.Virtual_File)
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits;
   --  Gnatformat library entry point for the whole Unit formatting

   function Diagnostics_Array_To_Vector
     (Arr : Diagnostics_Array) return Diagnostics_Vectors.Vector;
   --  Convert diagnostics array into dignostics vector

   procedure Find_Matching_Parents
     (Node     : Libadalang.Analysis.Ada_Node'Class;
      Match    : not null access function
        (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;
      Callback : not null access procedure
        (Parent : Libadalang.Analysis.Ada_Node;
         Stop   : in out Boolean));
   --  Iterates through the parents of Node and calls Callback on the
   --  parents where Match returns True. This iterative process stops if
   --  Callback sets Stop to True.

   procedure Get_Selection_Enclosing_Node
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      SL_Range       : Langkit_Support.Slocs.Source_Location_Range;
      Enclosing_Node : out Libadalang.Analysis.Ada_Node);
   --  Retrieves the closest enclosing parent for the given selection range

   function Get_Initial_Indentation
     (Node        : Libadalang.Analysis.Ada_Node;
      Indentation : Natural := 3)
      return Natural;
   --  Returns the initial indentation that needs to be used for the selected
   --  Node formatting

   function Estimate_Indentation
     (Node               : Libadalang.Analysis.Ada_Node;
      Indentation        : Natural := 3;
      Inline_Indentation : Natural := 2)
      return Natural;
   --  Estimate the indentation for Node (assuming that it starts in the
   --  begining of its start line.

   -----------------------------------
   --  Diagnostics_Array_To_Vector  --
   -----------------------------------

   function Diagnostics_Array_To_Vector
     (Arr : Diagnostics_Array) return Diagnostics_Vectors.Vector
   is
      V : Diagnostics_Vectors.Vector := Diagnostics_Vectors.Empty_Vector;
   begin
      for I in Arr'Range loop
         Append
           (Diagnostics => V,
            Sloc_Range  => Arr (I).Sloc_Range,
            Message     => Langkit_Support.Text.To_Text (Arr (I).Message));
      end loop;
      return V;
   end Diagnostics_Array_To_Vector;

   -------------------
   --  Format_Unit  --
   -------------------

   function Format_Unit
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      use Ada.Strings.Unbounded;

      SL_Root        : constant Source_Location_Range := Unit.Root.Sloc_Range;

      Formatted_Str  : constant Unbounded_String :=
        Format (Unit => Unit,
                Format_Options => Options,
                Configuration  => Unparsing_Config);

      Diagnostics_V  : constant Diagnostics_Vectors.Vector :=
        Diagnostics_Array_To_Vector (Unit.Diagnostics);
   begin
      return Formatted_Edits'
        (Unit => Unit,
         Edit =>
           Text_Edit'(Location => SL_Root,
                      Text     => Formatted_Str),
         Formatted   => Unit.Root,
         Diagnostics => Diagnostics_V);
   end Format_Unit;

   ---------------------------
   -- Find_Matching_Parents --
   ---------------------------

   procedure Find_Matching_Parents
     (Node     : Libadalang.Analysis.Ada_Node'Class;
      Match    : not null access function
        (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;
      Callback : not null access procedure
        (Parent : Libadalang.Analysis.Ada_Node;
         Stop   : in out Boolean))
   is
      use Libadalang.Analysis;
      Parent : Ada_Node :=
        (if Node.Is_Null then No_Ada_Node else Node.Parent);
      Stop   : Boolean := False;

   begin
      while not Stop loop
         exit when Parent.Is_Null;

         if Match (Parent) then
            Callback (Parent, Stop);
         end if;

         Parent := Parent.Parent;
      end loop;
   end Find_Matching_Parents;

   ------------------------------------------
   --  Get_Selected_Region_Enclosing_Node  --
   ------------------------------------------

   procedure Get_Selection_Enclosing_Node
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      SL_Range       : Langkit_Support.Slocs.Source_Location_Range;
      Enclosing_Node : out Libadalang.Analysis.Ada_Node)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Langkit_Support.Slocs;

      type Search_Direction is (Forward, Backward);

      function Lookup (Unit  : Analysis_Unit;
                       Token : Libadalang.Common.Token_Reference;
                       Look  : Search_Direction)
                       return Ada_Node;
      --  Finds the next Ada_Node relative to Token. Look param controls the
      --  search direction. If Token already belongs to an Ada_Node, that node
      --  is returned. Returns No_Ada_Node if no node is found or if
      --  Token = No_Token.

      ------------
      -- Lookup --
      ------------

      function Lookup
        (Unit  : Analysis_Unit;
         Token : Libadalang.Common.Token_Reference;
         Look  : Search_Direction)
      return Ada_Node
      is
         Crt_Token      : Token_Reference              := Token;
         Crt_Token_Kind : Libadalang.Common.Token_Kind :=
           Kind (Libadalang.Common.Data (Crt_Token));
      begin
         --  Nothing to do if Aux_Token <=> Token is a No_Token or already
         --  belongs to an Ada_Node.
         while not (Crt_Token = No_Token)
           and then Crt_Token_Kind in Ada_Comment | Ada_Whitespace
         loop
            case Look is
               when Forward =>
                  Crt_Token := Next (Crt_Token);

               when Backward =>
                  Crt_Token := Previous (Crt_Token);
            end case;
            Crt_Token_Kind := Kind (Data (Crt_Token));
         end loop;

         if Crt_Token = No_Token then
            return No_Ada_Node;
         end if;

         return Unit.Root.Lookup
           (Start_Sloc (Sloc_Range (Data (Crt_Token)))).As_Ada_Node;
      end Lookup;

      Crt_Start_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.Start_Line, SL_Range.Start_Column));
      Crt_End_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.End_Line, SL_Range.End_Column));

      Crt_Start_Node : constant Ada_Node :=
        Lookup (Unit, Crt_Start_Tok, Forward);
      Crt_End_Node   : constant Ada_Node :=
        Lookup (Unit, Crt_End_Tok, Backward);

      --  This is a variable used to find the first relevant parent of
      --  Crt_Start_Node and Crt_End_Node
      Parent_Node : Ada_Node := No_Ada_Node;

      function Is_Relevant_Parent_Kind
        (Kind : Ada_Node_Kind_Type) return Boolean
      is (Kind in Ada_Decl_Block | Ada_Type_Decl | Ada_Compilation_Unit
            | Ada_Stmt | Ada_Stmt_List | Ada_Ada_Node_List
            | Ada_Basic_Decl | Ada_Subp_Spec
            | Ada_Use_Type_Clause);

      function Is_Relevant_Parent_Node
        (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Is_Relevant_Parent_Kind (Node.Kind));

      procedure Is_Relevant_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node.

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean;
      --  Returns True if one of these nodes is already overlapping the other.

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
      --  Returns the overlapping node.

      function Get_Common_Enclosing_Parent_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
      --  Starting from 2 given nodes, get the first enclosing common parent

      --------------------------------------
      -- Is_Relevant_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Relevant_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean)
      is
      begin
         Stop        := True;
         Parent_Node := Parent;
      end Is_Relevant_Parent_Node_Callback;

      -------------------------------
      -- Are_Overlapping_Nodes --
      -------------------------------

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean
      is
         pragma Assert (Start_Node /= End_Node);
      begin
         return
           (Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then
            Start_Node.Sloc_Range.End_Line < End_Node.Sloc_Range.End_Line)
           or else
             (Start_Node.Sloc_Range.Start_Line < End_Node.Sloc_Range.Start_Line
              and then
              Start_Node.Sloc_Range.End_Line > End_Node.Sloc_Range.End_Line);
      end Are_Overlapping_Nodes;

      --------------------------
      -- Get_Overlapping_Node --
      --------------------------

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node
      is
         pragma Assert (Start_Node /= End_Node
                        and then Are_Overlapping_Nodes (Start_Node, End_Node));
      begin
         if Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then
             Start_Node.Sloc_Range.End_Line < End_Node.Sloc_Range.End_Line
         then
            return End_Node;

         elsif
           Start_Node.Sloc_Range.Start_Line < End_Node.Sloc_Range.Start_Line
           and then
             Start_Node.Sloc_Range.End_Line > End_Node.Sloc_Range.End_Line
         then
            return Start_Node;
         end if;

         return No_Ada_Node;
      end Get_Overlapping_Node;

      ----------------------------------------
      --  Get_Common_Enclosing_Parent_Node  --
      ----------------------------------------

      function Get_Common_Enclosing_Parent_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node
      is
         pragma Assert
           (Start_Node /= No_Ada_Node and then End_Node /= No_Ada_Node);
      begin
         if Start_Node = End_Node then
            return Start_Node;
         else
            declare
               Start_Parents : constant Ada_Node_Array := Start_Node.Parents;
               End_Parents   : constant Ada_Node_Array := End_Node.Parents;
            begin
               for Crt_Parent of Start_Parents loop
                  for I of End_Parents loop
                     if Crt_Parent = I then
                        return I.As_Ada_Node;
                     end if;
                  end loop;
               end loop;
            end;
         end if;

         return No_Ada_Node;
      end Get_Common_Enclosing_Parent_Node;

      Start_Node, End_Node : Ada_Node := No_Ada_Node;

      --  Start of Get_Selection_Enclosing_Node
   begin
      Enclosing_Node := No_Ada_Node;
      Parent_Node    := Crt_Start_Node;

      --  Find the first relevant parent of Crt_Start_Node
      if not Is_Relevant_Parent_Kind (Kind (Crt_Start_Node)) then
         Find_Matching_Parents
           (Crt_Start_Node,
            Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      Start_Node := Parent_Node.As_Ada_Node;

      --  Find the first relevant parent of Crt_End_Node
      Parent_Node := Crt_End_Node.As_Ada_Node;

      if not Is_Relevant_Parent_Kind (Kind (Crt_End_Node)) then
         Find_Matching_Parents
           (Crt_End_Node,
            Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      End_Node := Parent_Node.As_Ada_Node;

      --  When the selection contains different parts of different nodes,
      --  find the first encolsing parent node, otherwise the Enclosing_Node
      --  will be equal to Start_Node or End_Node in some situations.
      if Start_Node /= End_Node then
         if Are_Overlapping_Nodes (Start_Node, End_Node) then
            Enclosing_Node := Get_Overlapping_Node (Start_Node, End_Node);
         else
            Enclosing_Node :=
              Get_Common_Enclosing_Parent_Node (Start_Node, End_Node);

            Parent_Node := Enclosing_Node;

            if Enclosing_Node /= No_Ada_Node
              and then not Is_Relevant_Parent_Kind (Kind (Enclosing_Node))
            then
               Find_Matching_Parents
                 (Enclosing_Node,
                  Is_Relevant_Parent_Node'Access,
                  Is_Relevant_Parent_Node_Callback'Access);
               Enclosing_Node := Parent_Node.As_Ada_Node;
            end if;
         end if;
         pragma Assert (Enclosing_Node /= No_Ada_Node);

      else
         Enclosing_Node := Start_Node;
      end if;

   end Get_Selection_Enclosing_Node;

   -------------------------------
   --  Get_Initial_Indentation  --
   -------------------------------

   function Get_Initial_Indentation
     (Node        : Libadalang.Analysis.Ada_Node;
      Indentation : Natural := 3)
     return Natural
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Langkit_Support.Slocs;

      Parent_Node : Ada_Node := No_Ada_Node;

      function Get_Parent_Indentation (Node : Ada_Node) return Natural;
      --  Returns the Node's parent indentation

      function Is_Expected_Parent_Kind
        (Kind : Ada_Node_Kind_Type) return Boolean
      is (Kind in Ada_Package_Body | Ada_Package_Decl |
                  Ada_Library_Item | Ada_Subp_Body | Ada_Task_Body |
                  Ada_Decl_Block | Ada_For_Loop_Stmt | Ada_Loop_Stmt |
                  Ada_While_Loop_Stmt | Ada_If_Stmt_Range |
                  Ada_Case_Stmt_Range | Ada_Case_Stmt_Alternative_Range);

      function Is_Expected_Parent_Node
        (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Is_Expected_Parent_Kind (Node.Kind));

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node

      --------------------------------------
      -- Is_Expected_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean)
      is
      begin
         Stop        := True;
         Parent_Node := Parent;
      end Is_Expected_Parent_Node_Callback;

      ---------------------------
      -- Get_Parent_Indenation --
      ---------------------------

      function Get_Parent_Indentation (Node : Ada_Node) return Natural
      is
         Offset : Natural := 0;
      begin
         Parent_Node := Node;
         Find_Matching_Parents (Node,
                                Is_Expected_Parent_Node'Access,
                                Is_Expected_Parent_Node_Callback'Access);

         if Kind (Parent_Node) = Ada_Library_Item
           and then Natural (Parent_Node.Sloc_Range.Start_Line) = 1
           and then Natural (Parent_Node.Sloc_Range.Start_Column) > 0
         then
            Offset := 0;
         else
            Offset := Natural (Parent_Node.Sloc_Range.Start_Column) - 1;
         end if;

         case Kind (Parent_Node) is
            when Ada_Package_Body | Ada_Package_Decl
               | Ada_Task_Body | Ada_Subp_Body | Ada_Decl_Block
               | Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt
               | Ada_If_Stmt_Range | Ada_Case_Stmt_Range
               | Ada_Case_Stmt_Alternative_Range
               => Offset := Offset + Indentation;

            when others => null;
         end case;

         return Offset;
      end Get_Parent_Indentation;

      ----------------------------
      --  Get_Previous_Sibling  --
      ----------------------------

      function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node
      is (if Node /= No_Ada_Node then Node.Previous_Sibling.As_Ada_Node
          else No_Ada_Node);
      --  Returns the Node previous sibling or No_Ada_Node if no sibling found

      ------------------------
      --  Get_Next_Sibling  --
      ------------------------

      function Get_Next_Sibling (Node : Ada_Node) return Ada_Node
      is (if Node /= No_Ada_Node then Node.Next_Sibling.As_Ada_Node
          else No_Ada_Node);
      --  Returns the Node next sibling or No_Ada_Node if no sibling found

      Prev_Sibling : constant Ada_Node := Get_Previous_Sibling (Node);
      Next_Sibling : constant Ada_Node := Get_Next_Sibling (Node);
      Offset       : Natural := 0;

   begin
      if Node.Kind in Ada_Ada_List then
         Offset :=
           (if Node.Sloc_Range.Start_Column = 0 then 0
            else Natural (Node.Sloc_Range.Start_Column) - 1);

      elsif Node.Kind in Ada_Subp_Spec_Range then
         --  Subp_Spec nodes can have an overriding node sibling. The correct
         --  offset is given by the enclosing declaration, which is the
         --  parent node.
         Offset :=
           Get_Initial_Indentation
             (Node.P_Parent_Basic_Decl.As_Ada_Node, Indentation);

      elsif (not Prev_Sibling.Is_Null and not Next_Sibling.Is_Null)
        and then Prev_Sibling.Sloc_Range.Start_Column =
                   Next_Sibling.Sloc_Range.Start_Column
      then
         Offset := (if Prev_Sibling.Sloc_Range.Start_Column = 0 then 0
                    else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);

      elsif not Prev_Sibling.Is_Null then
         if Node.Kind in Ada_Subp_Body | Ada_Package_Body | Ada_Package_Decl
                       | Ada_Generic_Package_Renaming_Decl
         then
            if Prev_Sibling.Kind = Ada_Private_Absent
              and then Next_Sibling.Is_Null
            then
               --  Get the parent node which should be a Library_Item which
               --  will give us the offset to use for the reformatting
               Offset := Get_Parent_Indentation (Node);
            else
               Offset :=
                 (if Prev_Sibling.Sloc_Range.Start_Column = 0 then 0
                  else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);
            end if;
         else
            Offset :=
              (if Prev_Sibling.Sloc_Range.Start_Column = 0 then 0
               else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);
         end if;

      elsif not Next_Sibling.Is_Null then
         Offset := (if Next_Sibling.Sloc_Range.Start_Column = 0 then 0
                    else Natural (Next_Sibling.Sloc_Range.Start_Column) - 1);

      elsif Prev_Sibling.Is_Null and Next_Sibling.Is_Null then
         --  We should look backward for the Node parent to find the offset
         --  of the parent and compute the one related to the reformatted node
         --  based on gnatpp indentation and indent continuation parameters
         Offset := Get_Parent_Indentation (Node);

      else
         Offset := (if Node.Sloc_Range.Start_Column = 0 then 0
                    else Natural (Node.Sloc_Range.Start_Column) - 1);
      end if;

      return Offset;
   end Get_Initial_Indentation;

   --------------------------
   -- Estimate_Indentation --
   --------------------------

   function Estimate_Indentation
     (Node               : Libadalang.Analysis.Ada_Node;
      Indentation        : Natural := 3;
      Inline_Indentation : Natural := 2)
      return Natural
   is
      use Libadalang.Analysis;

      function Parent_Based_Indentation
        (Parents            : Ada_Node_Array;
         Indentation        : Positive := 3;
         Inline_Indentation : Positive := 2)
      return Natural;
      --  Computes Indentation starting at zero and incrementing based on the
      --  Parents kind or returning earlier if finds a parent that always sets
      --  indentation, for instance, a parameter list.

      ------------------------------
      -- Parent_Based_Indentation --
      ------------------------------

      function Parent_Based_Indentation
        (Parents            : Ada_Node_Array;
         Indentation        : Positive := 3;
         Inline_Indentation : Positive := 2)
      return Natural
      is
         use Libadalang.Common;

         Current_Indentation : Natural := 0;
      begin
         for Parent of Parents loop
            case Parent.Kind is
            when Ada_Loop_Stmt_Range
               | Ada_For_Loop_Stmt_Range
               | Ada_While_Loop_Stmt_Range
               | Ada_If_Stmt_Range
               | Ada_Case_Stmt_Range
               | Ada_Case_Stmt_Alternative_Range
               | Ada_Record_Type_Def_Range
               | Ada_Generic_Formal_Part_Range
               | Ada_Begin_Block_Range
               | Ada_Decl_Block_Range =>
               Current_Indentation := @ + Indentation;

            when Ada_Declarative_Part_Range =>
               --  When we type declare, a DeclBlock is created but not a
               --  DeclarativePart one. Only when you close the block with an
               --  end the node is created.
               --  DeclarativePart is a node that adds indentation.
               --  We cannot simply make DeclBlock also add indentation because
               --  it would double indent. So only add indentation to
               --  DeclarativeParts if their parent is not  DeclBlock.
               if Parent.Parent.Kind not in Ada_Decl_Block_Range then
                  Current_Indentation := @ + Indentation;
               end if;

            when Ada_Handled_Stmts_Range =>
               --  HandledStmts can be children of DeclBlock and BeginBlock.
               --  These two add indentation, so HandledStmts should not
               --  double add if its their child.
               if Parent.Parent.Kind not in
                 Ada_Begin_Block_Range | Ada_Decl_Block_Range
               then
                  Current_Indentation := @ + Indentation;
               end if;

            when Ada_Subp_Spec_Range | Ada_Assign_Stmt_Range =>
               Current_Indentation := @ + Inline_Indentation;

            when Ada_Dotted_Name_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1
                 + Inline_Indentation;
               exit;

               when Ada_Params_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1 + 1;
               exit;

               when Ada_Assoc_List_Range | Ada_Component_List_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1;
               exit;

               when others =>
               null;
            end case;
         end loop;

         return Current_Indentation;
      end Parent_Based_Indentation;

      Parents    : constant Ada_Node_Array :=
        (if Node.Is_Null then [] else Node.Parents (False));

   begin
      return
        Parent_Based_Indentation (Parents, Indentation, Inline_Indentation);
   end Estimate_Indentation;

   --------------------
   --  Range_Format  --
   --------------------

   function Range_Format
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Input_Selection_Range : Langkit_Support.Slocs.Source_Location_Range;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      --  use Ada.Strings.Unbounded;

      Enclosing_Node        : Ada_Node := No_Ada_Node;
      Initial_Indentation   : Natural  := 0;
      Indentation_Offset    : Indentation_Offset_Type := (0, 0);
      Estimated_Indentation : Natural := 0;

      Format_Options        : Prettier_Ada.Documents.Format_Options_Type :=
        Options.Into (Ada_Language);
      Offset_Set       : Boolean := False;
   begin
      if Input_Selection_Range = No_Source_Location_Range then
      --  If no selection range is provided dispatch to format the whole unit
         return Format_Unit (Unit             => Unit,
                             Options          => Options,
                             Unparsing_Config => Unparsing_Config);
      end if;

      --  If an input selection is provided then follow the steps to get a
      --  partial formatting of the file for the encolsing node of the given
      --  selection

      --  1. Find the corresponding enclosing node given the initial
      --     selection range.

      Get_Selection_Enclosing_Node
        (Unit           => Unit,
         SL_Range       => Input_Selection_Range,
         Enclosing_Node => Enclosing_Node);
      pragma Assert (Enclosing_Node /= No_Ada_Node);

      --  2. Compute the offset for the indentation of the enclosing node
      --     based on the previous or next sibling starting column position and
      --     also the estimated indentation. If these are different use the
      --     estimated value instead of the initial indetation since if the
      --     indentations of siblings are wrong we get a wrong value for it.
      --     and set this value for further usage by prettier.

      declare
         use Ada.Directories;
         Current_Indentation         : constant Natural :=
           Gnatformat.Configuration.Get_Indentation
             (Self              => Options,
              Source_Filename   => Simple_Name (Unit.Get_Filename),
              Language_Fallback => Ada_Language);

         Current_Continuation_Indent : constant Natural :=
           Gnatformat.Configuration.Get_Indentation_Continuation
             (Self              => Options,
              Source_Filename   => Simple_Name (Unit.Get_Filename),
              Language_Fallback => Ada_Language);
      begin
         Initial_Indentation := Get_Initial_Indentation
           (Node        => Enclosing_Node,
            Indentation => Current_Indentation);

         Estimated_Indentation := Estimate_Indentation
           (Node               => Enclosing_Node,
            Indentation        => Current_Indentation,
            Inline_Indentation => Current_Continuation_Indent);

         if Initial_Indentation /= Estimated_Indentation then
            Initial_Indentation := Estimated_Indentation;
         end if;

         --  Update the Indentation_Offset value of Format_Opt using the
         --  computed offset; this updated value is passed to prettier as
         --  format options when calling Prettier_Ada.Documents.Format at the
         --  next step with the document resulting from the unparsing of the
         --  Enclosing_Node.

         if Format_Options.Indentation.Kind = Spaces then
            Indentation_Offset := (Tabs => 0, Spaces => Initial_Indentation);
         else
            --  Convert the offset into tabs usage compatible value
            Indentation_Offset :=
              (Tabs   =>
                 Initial_Indentation / Format_Options.Indentation.Width,
               Spaces =>
                 Initial_Indentation mod Format_Options.Indentation.Width);
         end if;

         if Format_Options.Indentation.Offset /= Indentation_Offset then
            Format_Options.Indentation.Offset := Indentation_Offset;
            Offset_Set := True;
         end if;
      end;

      --  3. Rewrite the enclosing node relative to the input selection and
      --     return the formatted edits with the new SLOC related to the text
      --     edit to be used by the IDE's for the range formatting.
      declare
         use Langkit_Support.Generic_API;

         Document : constant Prettier_Ada.Documents.Document_Type :=
           Unparsing.Unparse_To_Prettier
             (Libadalang.Generic_API.To_Generic_Node (Enclosing_Node),
              Unparsing_Config);

         Diagnostics     : constant Diagnostics_Vectors.Vector :=
           Diagnostics_Array_To_Vector (Unit.Diagnostics);
         Text_Edit_Sloc  : Langkit_Support.Slocs.Source_Location_Range :=
           Enclosing_Node.Sloc_Range;
      begin
         if Offset_Set and then Enclosing_Node.Sloc_Range.Start_Column /= 1
         then
            --  Reset the formatted node start column position to be able to
            --  get the right indented text edit (to trim the spaces added as
            --  offset) when the formatted node is rewritten
            Text_Edit_Sloc.Start_Column := 1;
         end if;

         return Formatted_Edits'
           (Unit => Unit,
            Edit =>
              Text_Edit'
                (Location => Text_Edit_Sloc,
                 Text     =>
                   Prettier_Ada.Documents.Format (Document, Format_Options)),
            Formatted   => Enclosing_Node,
            Diagnostics => Diagnostics);
      end;
   end Range_Format;

   -----------
   -- Image --
   -----------

   function Image (Edit : Formatted_Edits) return String is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Langkit_Support.Slocs;
   begin
      return
        Simple_Name (Edit.Unit.Get_Filename)
        & "("
        & Edit.Formatted.Image
        & ") - "
        & Image (Edit.Edit.Location)
        & Ada.Characters.Latin_1.LF
        & '^'
        & Ada.Characters.Latin_1.LF
        & To_String (Edit.Edit.Text)
        & '$';
   end Image;

end Gnatformat.Formatting;
