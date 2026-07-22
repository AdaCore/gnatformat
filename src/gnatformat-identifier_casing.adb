--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gnatformat.Edits;
with Gnatformat.Formatting;

with Langkit_Support.Slocs;
with Langkit_Support.Symbols;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Sources;

package body Gnatformat.Identifier_Casing is

   ----------------------------------
   -- Normalize_Identifier_Casing --
   ----------------------------------

   function Normalize_Identifier_Casing
     (Unit : Libadalang.Analysis.Analysis_Unit)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Libadalang.Analysis;

      No_Canonical_Spelling : constant Text_Type := "";
      --  Sentinel returned by Canonical_Spelling when the occurrence has no
      --  canonical defining-name spelling to apply: an unresolved or ambiguous
      --  reference, an operator / character-literal / synthetic name, or a
      --  Property_Error. A real spelling is never empty, so this is
      --  unambiguous.

      function Canonical_Spelling
        (Identifier_Node : Identifier) return Text_Type;
      --  Returns the spelling of the canonical defining name that
      --  Identifier_Node refers to or is part of. Returns
      --  No_Canonical_Spelling when it should be left unchanged (unresolved,
      --  ambiguous, or not an identifier-spelled entity).

      function Same_Identifier (Left, Right : Text_Type) return Boolean;
      --  Whether Left and Right denote the same Ada identifier up to case.
      --  Uses Libadalang's own canonicalization (case folding plus bracket
      --  decoding), so the result matches how the compiler considers two
      --  identifiers equivalent.

      ------------------------
      -- Canonical_Spelling --
      ------------------------

      function Canonical_Spelling
        (Identifier_Node : Identifier) return Text_Type
      is
         Name_Node  : constant Name := Identifier_Node.As_Name;
         Definition : Defining_Name;

      begin
         if Name_Node.P_Is_Defining then
            Definition := Name_Node.P_Enclosing_Defining_Name;

         else
            Definition :=
              Name_Node.P_Referenced_Defining_Name
                (Imprecise_Fallback => False);
         end if;

         if Definition.Is_Null then
            return No_Canonical_Spelling;
         end if;

         declare
            Canonical_Definition : Defining_Name :=
              Definition.P_Canonical_Part (Imprecise_Fallback => False);
            Canonical_Name       : Name;

         begin
            if Canonical_Definition.Is_Null then
               Canonical_Definition := Definition;
            end if;

            Canonical_Name := Canonical_Definition.F_Name;

            if Canonical_Name.Is_Null then
               return No_Canonical_Spelling;
            end if;

            case Canonical_Name.Kind is
               when Ada_Identifier  =>
                  return Canonical_Name.Text;

               when Ada_Dotted_Name =>
                  return Canonical_Name.As_Dotted_Name.F_Suffix.Text;

               when others          =>
                  --  Operators, character literals, synthetic names: leave the
                  --  occurrence unchanged.
                  return No_Canonical_Spelling;
            end case;
         end;

      exception
         when Property_Error =>
            return No_Canonical_Spelling;
      end Canonical_Spelling;

      ---------------------
      -- Same_Identifier --
      ---------------------

      function Same_Identifier (Left, Right : Text_Type) return Boolean is
         use Langkit_Support.Symbols;

         Left_Fold  : constant Symbolization_Result :=
           Libadalang.Sources.Canonicalize (Left);
         Right_Fold : constant Symbolization_Result :=
           Libadalang.Sources.Canonicalize (Right);

      begin
         return
           Left_Fold.Success
           and then Right_Fold.Success
           and then Left_Fold.Symbol = Right_Fold.Symbol;
      end Same_Identifier;

      Source : constant Unbounded_String :=
        To_Unbounded_String (To_UTF8 (Unit.Text));

      Edits : Gnatformat.Edits.Text_Edit_Ordered_Set;
      Token : Token_Reference;

   begin
      --  Walk every identifier token, collecting a case-only edit for each one
      --  whose spelling differs from its canonical defining name. The edits
      --  are then applied onto Unit's source text, leaving all other bytes
      --  untouched.

      Token := Unit.First_Token;
      while Token /= No_Token loop
         if Kind (Data (Token)) = Ada_Identifier then
            declare
               Sloc : constant Langkit_Support.Slocs.Source_Location :=
                 Langkit_Support.Slocs.Start_Sloc (Sloc_Range (Data (Token)));
               Node : constant Ada_Node := Unit.Root.Lookup (Sloc);

            begin
               if not Node.Is_Null and then Node.Kind = Ada_Identifier then
                  declare
                     Token_Text : constant Text_Type :=
                       Libadalang.Common.Text (Token);
                     Canonical  : constant Text_Type :=
                       Canonical_Spelling (Node.As_Identifier);

                  begin
                     --  No_Canonical_Spelling means the occurrence could not
                     --  be resolved. Such occurrences are left unchanged.

                     if Canonical /= No_Canonical_Spelling
                       and then Same_Identifier (Token_Text, Canonical)
                     then
                        if Token_Text /= Canonical then
                           Edits.Insert
                             (Gnatformat.Edits.Text_Edit_Type'
                                (Location => Sloc_Range (Data (Token)),
                                 Text     =>
                                   To_Unbounded_String (To_UTF8 (Canonical))));
                        end if;

                     elsif Canonical /= No_Canonical_Spelling then
                        Gnatformat_Trace.Trace
                          ("Identifier casing: occurrence """
                           & To_UTF8 (Token_Text)
                           & """ at "
                           & Langkit_Support.Slocs.Image (Sloc)
                           & " in "
                           & Unit.Get_Filename
                           & " resolves to """
                           & To_UTF8 (Canonical)
                           & """ which differs by more than case; leaving "
                           & "this occurrence unchanged");
                     end if;
                  end;
               end if;
            end;
         end if;

         Token := Next (Token);
      end loop;

      if Edits.Is_Empty then
         return Source;
      end if;

      return
        Gnatformat.Formatting.Restore_Off_On_Sections
          (Original_Source  => Source,
           Formatted_Source => Gnatformat.Edits.Apply_Edits (Source, Edits));

   exception
      when others =>
         return Source;
   end Normalize_Identifier_Casing;

   ---------------------
   -- Normalized_Unit --
   ---------------------

   function Normalized_Unit
     (Unit : Libadalang.Analysis.Analysis_Unit)
      return Libadalang.Analysis.Analysis_Unit
   is
      use Libadalang.Analysis;

      Reparse_Context : constant Analysis_Context := Create_Context;

      Normalized_Source : constant Ada.Strings.Unbounded.Unbounded_String :=
        Normalize_Identifier_Casing (Unit);

      Reparsed_Unit : constant Analysis_Unit :=
        Reparse_Context.Get_From_Buffer
          (Filename => Unit.Get_Filename,
           Charset  => "utf-8",
           Buffer   => Normalized_Source);

   begin
      if Reparsed_Unit.Has_Diagnostics then
         return Unit;
      else
         return Reparsed_Unit;
      end if;
   end Normalized_Unit;

end Gnatformat.Identifier_Casing;
