--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Directories;
with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regpat;

with Langkit_Support.Slocs;

with Gnatformat.Edits; use Gnatformat.Edits;
with Gnatformat.Formatting;

package body Gitdiff is
   package Slocs renames Langkit_Support.Slocs;

   package Fmt renames Gnatformat.Formatting;

   type Diff_Elem_Kind is (File, Hunk, None);

   type Diff_Elem (Kind : Diff_Elem_Kind := None) is record
      case Kind is
         when File =>
            File_Name : Unbounded_String;

         when Hunk =>
            First_Line, Last_Line : Positive;

         when None =>
            null;
      end case;
   end record;

   function Contains
     (Possibly_Contained, Possibly_Containing : Slocs.Source_Location_Range)
      return Boolean;

   function Contains
     (Possibly_Contained, Possibly_Containing : Slocs.Source_Location_Range)
      return Boolean
   is
      use Slocs;
   begin
      return
        Start_Sloc (Possibly_Containing) <= Start_Sloc (Possibly_Contained)
        and then End_Sloc (Possibly_Contained)
                 <= End_Sloc (Possibly_Containing);
   end Contains;

   function Are_Disjoint
     (Sloc1, Sloc2 : Slocs.Source_Location_Range) return Boolean;

   function Are_Disjoint
     (Sloc1, Sloc2 : Slocs.Source_Location_Range) return Boolean
   is
      use Slocs;
   begin
      return
        End_Sloc (Sloc1) < Start_Sloc (Sloc2)
        or else End_Sloc (Sloc2) < Start_Sloc (Sloc1);
   end Are_Disjoint;

   procedure Insert_Edit
     (Set : in out Text_Edit_Ordered_Set; Edit : Text_Edit_Type);
   --  Since the source location ranges of our text edits come from syntactic
   --  libadalang nodes, we make the assumption that if two ranges intersect,
   --  then one of them is contained in the other. From there, we can arrange
   --  for the elements in an edit set to be pairwise disjoint without missing
   --  any spots.

   procedure Insert_Edit
     (Set : in out Text_Edit_Ordered_Set; Edit : Text_Edit_Type)
   is
      use Text_Edit_Ordered_Sets;
      C : Cursor := First (Set);
   begin
      while Has_Element (C) loop
         declare
            S      : constant Slocs.Source_Location_Range :=
              Element (C).Location;
            Next_C : constant Cursor := Next (C);
         begin
            if Contains (Edit.Location, S) then
               return;
            elsif Contains (S, Edit.Location) then
               Set.Delete (C);
            else
               pragma Assert (Are_Disjoint (Edit.Location, S));
            end if;

            C := Next_C;
         end;
      end loop;

      Set.Include (Edit);
   end Insert_Edit;

   Git_Command_Failed : exception;

   procedure CD_To_Toplevel;
   --  This is inspired by and equivalent to the function of the same name in
   --  git-clang-format.

   procedure CD_To_Toplevel is
      Args : constant GNAT.OS_Lib.Argument_List :=
        (new String'("rev-parse"), new String'("--show-toplevel"));

      Status : aliased Integer;

      Directory : constant String :=
        GNAT.Expect.Get_Command_Output
          (Command    => "git",
           Arguments  => Args,
           Input      => "",
           Status     => Status'Access,
           Err_To_Out => False);
   begin
      if Status /= 0 then
         raise Git_Command_Failed with "git command failed";
      end if;

      Ada.Directories.Set_Directory (Directory);
   end CD_To_Toplevel;

   function Get_Next
     (Diff_Text : String;
      Cursor    : in out Positive;
      Matcher   : GNAT.Regpat.Pattern_Matcher) return Diff_Elem;

   function Get_Next
     (Diff_Text : String;
      Cursor    : in out Positive;
      Matcher   : GNAT.Regpat.Pattern_Matcher) return Diff_Elem
   is
      use GNAT.Regpat;

      Matches : Match_Array (0 .. 5);
   begin
      loop
         Match (Matcher, Diff_Text, Matches, Cursor);

         if Matches (0) = No_Match then
            return (Kind => None);
         end if;

         Cursor := Matches (0).Last + 1;

         if Matches (1) /= No_Match then
            --  The first option was matched: we have a new file.
            return
              (Kind      => File,
               File_Name =>
                 To_Unbounded_String
                   (Diff_Text (Matches (2).First .. Matches (2).Last)));
         elsif Matches (3) /= No_Match then
            --  The second option was matched: we have a hunk for the current
            --  file.
            declare
               First_Line : constant Natural :=
                 Positive'Value
                   (Diff_Text (Matches (4).First .. Matches (4).Last));

               Line_Count : constant Natural :=
                 (if Matches (5) = No_Match
                  then 1
                  else
                    Natural'Value
                      (Diff_Text (Matches (5).First .. Matches (5).Last)));
            begin
               --  The meaning of the first new line in git diff's output when
               --  there are no new lines is unclear. In particular, it is
               --  sometimes 0, with which we cannot build a Diff_Elem. Since
               --  there is nothing to format anyway in that case, we simply
               --  skip the hunk.
               if Line_Count = 0 then
                  null;
               else
                  return (Hunk, First_Line, First_Line + Line_Count - 1);
               end if;
            end;
         else
            --  Something's wrong with git's output if we get here
            raise Git_Command_Failed;
         end if;
      end loop;
   end Get_Next;

   type File_State is record
      Unit : Libadalang.Analysis.Analysis_Unit;
      Set  : Text_Edit_Ordered_Set;
   end record;

   function Open (File_Name : String; Ctx : Context) return File_State;

   function Open (File_Name : String; Ctx : Context) return File_State is
   begin
      return X : File_State do
         X.Unit :=
           Ctx.Lal_Ctx.Get_From_File (File_Name, To_String (Ctx.Charset));
         X.Set := Text_Edit_Ordered_Sets.Empty_Set;
      end return;
   end Open;

   procedure Process_Hunk
     (State                 : in out File_State;
      First_Line, Last_Line : Positive;
      Ctx                   : Context);

   procedure Process_Hunk
     (State                 : in out File_State;
      First_Line, Last_Line : Positive;
      Ctx                   : Context)
   is
      --  We want to include the entire last line of the hunk in the range, but
      --  we don't know its length from what we parsed; fortunately we can use
      --  Libadalang's Get_Line to find out.
      End_Column : constant Natural := State.Unit.Get_Line (Last_Line)'Length;

      L1 : constant Slocs.Source_Location :=
        (Line => Slocs.Line_Number (First_Line), Column => 1);
      L2 : constant Slocs.Source_Location :=
        (Line   => Slocs.Line_Number (Last_Line),
         Column => Slocs.Column_Number (End_Column));

      Rnge : constant Slocs.Source_Location_Range := Slocs.Make_Range (L1, L2);

      Edit : constant Text_Edit_Type :=
        Fmt.Range_Format
          (State.Unit,
           Rnge,
           Ctx.Options,
           Ctx.Unparsing_Config,
           Use_Initial_Indentation => False)
          .Text_Edit;
   begin
      Insert_Edit (State.Set, Edit);
   end Process_Hunk;

   function Process_File
     (File_Name : String;
      Ctx       : Context;
      Map       : in out Formatting_Edits_Type;
      Diff_Text : String;
      Cursor    : in out Positive;
      Matcher   : GNAT.Regpat.Pattern_Matcher) return Unbounded_String;

   function Process_File
     (File_Name : String;
      Ctx       : Context;
      Map       : in out Formatting_Edits_Type;
      Diff_Text : String;
      Cursor    : in out Positive;
      Matcher   : GNAT.Regpat.Pattern_Matcher) return Unbounded_String
   is
      State : File_State := Open (File_Name, Ctx);

      E : Diff_Elem;
   begin
      loop
         E := Get_Next (Diff_Text, Cursor, Matcher);
         case E.Kind is
            when File =>
               Map.Insert (File_Name, State.Set);
               return E.File_Name;

            when Hunk =>
               Process_Hunk (State, E.First_Line, E.Last_Line, Ctx);

            when None =>
               Map.Insert (File_Name, State.Set);
               return Null_Unbounded_String;
         end case;
      end loop;
   end Process_File;

   --  The following regular expressions are inspired from the ones used by
   --  git-clang-format.
   File_Name_Regexp : constant String := "^\+\+\+ b/([^\n]+)$";

   Line_Span_Regexp : constant String := "^@@ [^\+]*\+(\d+)(?:,(\d+))?";

   Complete_Regexp : constant String :=
     "(" & File_Name_Regexp & ")|(" & Line_Span_Regexp & ")";

   Regpat_Flags : constant GNAT.Regpat.Regexp_Flags :=
     GNAT.Regpat.Multiple_Lines;

   procedure Format_New_Lines (Base_Commit_ID : String; Ctx : Context) is
      use GNAT.Regpat;

      Args : constant GNAT.OS_Lib.Argument_List :=
        (new String'("diff-index"),
         new String'("-p"),
         new String'("-U0"),
         new String'(Base_Commit_ID));

      Status : aliased Integer;

      Diff_Text : constant String :=
        GNAT.Expect.Get_Command_Output
          (Command    => "git",
           Arguments  => Args,
           Input      => "",
           Status     => Status'Access,
           Err_To_Out => False);

      Cursor : Positive := Diff_Text'First;

      Matcher : constant Pattern_Matcher :=
        GNAT.Regpat.Compile (Complete_Regexp, Regpat_Flags);

      E : Diff_Elem;

      File_Name : Unbounded_String := Null_Unbounded_String;

      Formatting_Edits : Formatting_Edits_Type :=
        Formatting_Edit_Hashed_Maps.Empty_Map;
   begin
      if Status /= 0 then
         raise Git_Command_Failed with "Failed to generate git diff";
      end if;

      E := Get_Next (Diff_Text, Cursor, Matcher);

      case E.Kind is
         when File =>
            File_Name := E.File_Name;

         when Hunk =>
            --  We never expect git's output to have a chunk before a file
            raise Git_Command_Failed with "Failed to parse diff text";

         when None =>
            return;
      end case;

      CD_To_Toplevel;

      Iterate_Over_Files :
      while File_Name /= Null_Unbounded_String loop
         declare
            use Ada.Directories;
            S : constant String := To_String (File_Name);
         begin
            if Extension (S) = "ads" or else Extension (S) = "adb" then

               File_Name :=
                 Process_File
                   (To_String (File_Name),
                    Ctx,
                    Formatting_Edits,
                    Diff_Text,
                    Cursor,
                    Matcher);
            else
               --  Ignore the current file's hunks
               loop
                  E := Get_Next (Diff_Text, Cursor, Matcher);

                  case E.Kind is
                     when File =>
                        File_Name := E.File_Name;
                        exit;

                     when Hunk =>
                        null;

                     when None =>
                        exit Iterate_Over_Files;
                  end case;
               end loop;
            end if;
         end;
      end loop Iterate_Over_Files;

      Apply_Edits (Formatting_Edits);

   end Format_New_Lines;
end Gitdiff;
