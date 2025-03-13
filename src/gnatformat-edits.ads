--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Package with the public API for editing sources

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;

with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;

with Libadalang.Analysis;

package Gnatformat.Edits is

   type Text_Edit_Type is record
      Location : Langkit_Support.Slocs.Source_Location_Range;
      Text     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   No_Text_Edit : constant Text_Edit_Type :=
     (Langkit_Support.Slocs.No_Source_Location_Range,
      Ada.Strings.Unbounded.Null_Unbounded_String);

   function "<" (L, R : Text_Edit_Type) return Boolean
   is (Langkit_Support.Slocs."<" (L.Location, R.Location));
   --  Checks if L is < than R, first based on the line number and then on
   --  the column number.

   package Text_Edit_Ordered_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Text_Edit_Type, "<" => "<");

   type Formatting_Edit_Type is record
      Unit           : Libadalang.Analysis.Analysis_Unit;
      Text_Edit      : Text_Edit_Type;
      Formatted_Node : Libadalang.Analysis.Ada_Node;
      Diagnostics    : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
   end record;

   function Image (Edit : Gnatformat.Edits.Formatting_Edit_Type) return String;

   subtype Text_Edit_Ordered_Set is Text_Edit_Ordered_Sets.Set;

   subtype Source_Path_Type is String;

   package Formatting_Edit_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Source_Path_Type,
        Element_Type    => Text_Edit_Ordered_Set,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => "=",
        "="             => Text_Edit_Ordered_Sets."=");

   subtype Formatting_Edits_Type is Formatting_Edit_Hashed_Maps.Map;

   procedure Apply_Edits (Edits : Formatting_Edits_Type);
   --  Applies Edits on disk

   procedure Insert_Text_Edit
     (Map         : in out Formatting_Edits_Type;
      Source_Path : Source_Path_Type;
      Text_Edit   : Text_Edit_Type);
   --  Associates Text_Edit to Source_Path

end Gnatformat.Edits;
