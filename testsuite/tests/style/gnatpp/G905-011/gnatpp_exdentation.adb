with Ada.Text_IO;
procedure Gnatpp_Exdentation is
  A_String_1   : constant String  := "abc";
  A_String_2   : constant String  := "def";
  A_String_3   : constant String  := "uvw";
  A_String_4   : constant String  := "xyz";
  An_Integer_1 : constant Integer := 1;
  An_Integer_2 : constant Integer := 2;
  An_Integer_3 : constant Integer := 3;
  An_Integer_4 : constant Integer := 4;
begin
  Ada.Text_IO.Put
    ("String concatenation:" & Ascii.LF
       & "1&2: " & A_String_1 & A_String_2 & Ascii.LF
       & "3&4: " & A_String_3 & A_String_4 & Ascii.LF);
  if An_Integer_1 /= An_Integer_2 and then
     An_Integer_3 /= An_Integer_4 and then
     An_Integer_1  = An_Integer_3
  then
    Ada.Text_IO.Put
     ("Computing addition: " & Ascii.LF
        & "1+2" & Integer'Image (An_Integer_1 + An_Integer_2) & Ascii.LF
        & "3+4" & Integer'Image (An_Integer_3 + An_Integer_4) & Ascii.LF);
  end if;
end Gnatpp_Exdentation;
