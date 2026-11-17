with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;

with Gnatformat.Edits; use Gnatformat.Edits;
with Gnatformat.Helpers;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

procedure Test is
   procedure Check (Ending : String);

   procedure Check (Ending : String) is
      Source   : constant Unbounded_String :=
        To_Unbounded_String
          ("éABC"
           & HT
           & "DEF"
           & HT
           & "GHI"
           & Ending
           & HT
           & "unchanged é"
           & HT
           & Ending);
      Expected : constant Unbounded_String :=
        To_Unbounded_String
          ("éa"
           & HT
           & "def"
           & HT
           & "ghi"
           & Ending
           & HT
           & "unchanged é"
           & HT
           & Ending);
      Edits    : Text_Edit_Ordered_Set;
      Files    : Formatting_Edits_Type;
   begin
      --  The two first edits end where a tab starts; columns count characters.
      Edits.Insert ((Make_Range ((1, 2), (1, 5)), To_Unbounded_String ("a")));
      Edits.Insert
        ((Make_Range ((1, 9), (1, 12)), To_Unbounded_String ("def")));
      Edits.Insert
        ((Make_Range ((1, 17), (1, 20)), To_Unbounded_String ("ghi")));
      pragma Assert (Apply_Edits (Source, Edits) = Expected);

      Gnatformat.Helpers.Write ("source.txt", Source);
      Files.Insert ("source.txt", Edits);
      Apply_Edits (Files);
      pragma
        Assert
          (Gnatformat.Helpers.Read_To_Unbounded_String ("source.txt")
           = Expected);
   end Check;
begin
   Check ("" & LF);
   Put_Line ("LF: memory and disk edits OK");
   Check (CR & LF);
   Put_Line ("CRLF: memory and disk edits OK");
end Test;
