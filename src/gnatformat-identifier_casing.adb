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
     (Unit   : Libadalang.Analysis.Analysis_Unit;
      Casing : Gnatformat.Configuration.Normalizing_Identifier_Casing_Kind)
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

      function Recased_Identifier
        (Identifier_Text : Text_Type;
         Casing          :
           Gnatformat.Configuration.Lexical_Identifier_Casing_Kind)
         return Text_Type;
      --  Returns Identifier_Text recased according to Casing: all lowercase,
      --  all uppercase, or mixed (the first letter and every letter
      --  following an underscore uppercase, all others lowercase).

      function Definition_Casing_Edits
         return Gnatformat.Edits.Text_Edit_Ordered_Set;
      --  Walks Unit's identifier tokens and returns a case-only edit for
      --  each occurrence whose spelling differs from the one of its
      --  canonical defining name.

      function Lexical_Casing_Edits
         return Gnatformat.Edits.Text_Edit_Ordered_Set;
      --  Walks Unit's identifier tokens and returns a case-only edit for
      --  each occurrence whose spelling differs from its lexically recased
      --  form (see Recased_Identifier).

      ------------------------
      -- Canonical_Spelling --
      ------------------------

      function Canonical_Spelling
        (Identifier_Node : Identifier) return Text_Type
      is
         Name_Node : constant Name := Identifier_Node.As_Name;

      begin
         declare
            Definition : constant Defining_Name :=
              (if Name_Node.P_Is_Defining
               then Name_Node.P_Enclosing_Defining_Name
               else
                 Name_Node.P_Referenced_Defining_Name
                   (Imprecise_Fallback => False));

         begin
            if Definition.Is_Null then
               return No_Canonical_Spelling;
            end if;

            declare
               Canonical_Definition : constant Defining_Name :=
                 (declare
                    Canonical_Definition_Candidate : constant Defining_Name :=
                      Definition.P_Canonical_Part
                        (Imprecise_Fallback => False);
                  begin
                    (if Canonical_Definition_Candidate.Is_Null
                     then Definition
                     else Canonical_Definition_Candidate));
               Canonical_Name       : constant Name :=
                 (if Canonical_Definition.Is_Null
                  then No_Name
                  else Canonical_Definition.F_Name);

            begin
               if Canonical_Name.Is_Null then
                  return No_Canonical_Spelling;
               end if;

               case Canonical_Name.Kind is
                  when Ada_Identifier  =>
                     return Canonical_Name.Text;

                  when Ada_Dotted_Name =>
                     return Canonical_Name.As_Dotted_Name.F_Suffix.Text;

                  when others          =>
                     --  Operators, character literals, synthetic names: leave
                     --  the occurrence unchanged.
                     return No_Canonical_Spelling;
               end case;
            end;
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

      ------------------------
      -- Recased_Identifier --
      ------------------------

      function Recased_Identifier
        (Identifier_Text : Text_Type;
         Casing          :
           Gnatformat.Configuration.Lexical_Identifier_Casing_Kind)
         return Text_Type is
      begin
         case Casing is
            when Gnatformat.Configuration.Lower =>
               return To_Lower (Identifier_Text);

            when Gnatformat.Configuration.Upper =>
               return To_Upper (Identifier_Text);

            when Gnatformat.Configuration.Mixed =>
               declare
                  Result          : Text_Type := Identifier_Text;
                  Capitalize_Next : Boolean := True;

               begin
                  for C of Result loop
                     if C = '_' then
                        Capitalize_Next := True;

                     elsif Capitalize_Next then
                        C := To_Upper (C);
                        Capitalize_Next := False;

                     else
                        C := To_Lower (C);
                     end if;
                  end loop;

                  return Result;
               end;
         end case;
      end Recased_Identifier;

      -----------------------------
      -- Definition_Casing_Edits --
      -----------------------------

      function Definition_Casing_Edits
         return Gnatformat.Edits.Text_Edit_Ordered_Set
      is
         Edits : Gnatformat.Edits.Text_Edit_Ordered_Set;
         Token : Token_Reference := Unit.First_Token;

      begin
         while Token /= No_Token loop
            if Kind (Data (Token)) = Ada_Identifier then
               declare
                  Sloc : constant Langkit_Support.Slocs.Source_Location :=
                    Langkit_Support.Slocs.Start_Sloc
                      (Sloc_Range (Data (Token)));
                  Node : constant Ada_Node := Unit.Root.Lookup (Sloc);

               begin
                  if not Node.Is_Null and then Node.Kind = Ada_Identifier then
                     declare
                        Token_Text : constant Text_Type :=
                          Libadalang.Common.Text (Token);
                        Canonical  : constant Text_Type :=
                          Canonical_Spelling (Node.As_Identifier);

                     begin
                        --  No_Canonical_Spelling means the occurrence could
                        --  not be resolved. Such occurrences are left
                        --  unchanged.

                        if Canonical /= No_Canonical_Spelling
                          and then Same_Identifier (Token_Text, Canonical)
                        then
                           if Token_Text /= Canonical then
                              Edits.Insert
                                (Gnatformat.Edits.Text_Edit_Type'
                                   (Location => Sloc_Range (Data (Token)),
                                    Text     =>
                                      To_Unbounded_String
                                        (To_UTF8 (Canonical))));
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
                              & """ which differs by more than case; "
                              & "leaving this occurrence unchanged");
                        end if;
                     end;
                  end if;
               end;
            end if;

            Token := Next (Token);
         end loop;

         return Edits;
      end Definition_Casing_Edits;

      --------------------------
      -- Lexical_Casing_Edits --
      --------------------------

      function Lexical_Casing_Edits
         return Gnatformat.Edits.Text_Edit_Ordered_Set
      is
         Edits : Gnatformat.Edits.Text_Edit_Ordered_Set;
         Token : Token_Reference := Unit.First_Token;

      begin
         while Token /= No_Token loop
            if Kind (Data (Token)) = Ada_Identifier then
               declare
                  Token_Text : constant Text_Type :=
                    Libadalang.Common.Text (Token);
                  Recased    : constant Text_Type :=
                    Recased_Identifier (Token_Text, Casing);

               begin
                  if Token_Text /= Recased then
                     Edits.Insert
                       (Gnatformat.Edits.Text_Edit_Type'
                          (Location => Sloc_Range (Data (Token)),
                           Text     =>
                             To_Unbounded_String (To_UTF8 (Recased))));
                  end if;
               end;
            end if;

            Token := Next (Token);
         end loop;

         return Edits;
      end Lexical_Casing_Edits;

      Source : constant Unbounded_String :=
        To_Unbounded_String (To_UTF8 (Unit.Text));

   begin
      declare
         --  Collect a case-only edit for every identifier token whose
         --  spelling differs from its target casing: the spelling of its
         --  canonical defining name for Definition, or its lexically recased
         --  form for Lower, Upper and Mixed. The edits are then applied onto
         --  Unit's source text, leaving all other bytes untouched.

         Edits : constant Gnatformat.Edits.Text_Edit_Ordered_Set :=
           (case Casing is
              when Gnatformat.Configuration.Definition =>
                Definition_Casing_Edits,
              when Gnatformat.Configuration.Lower
                 | Gnatformat.Configuration.Upper
                 | Gnatformat.Configuration.Mixed      =>
                Lexical_Casing_Edits);

      begin
         if Edits.Is_Empty then
            return Source;
         end if;

         return
           Gnatformat.Formatting.Restore_Off_On_Sections
             (Original_Source  => Source,
              Formatted_Source =>
                Gnatformat.Edits.Apply_Edits (Source, Edits));
      end;

   exception
      when others =>
         return Source;
   end Normalize_Identifier_Casing;

   ---------------------
   -- Normalized_Unit --
   ---------------------

   function Normalized_Unit
     (Unit   : Libadalang.Analysis.Analysis_Unit;
      Casing : Gnatformat.Configuration.Normalizing_Identifier_Casing_Kind)
      return Libadalang.Analysis.Analysis_Unit
   is
      use Libadalang.Analysis;

      Reparse_Context : constant Analysis_Context := Create_Context;

      Normalized_Source : constant Ada.Strings.Unbounded.Unbounded_String :=
        Normalize_Identifier_Casing (Unit, Casing);

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
