with Ada.Text_IO;

package body Test is

   use type
     Ada.Text_IO.Count;

   procedure You_Say_Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end;

   procedure I_Say_Goodbye is
   begin
      Ada.Text_IO.Put_Line ("Goodbye");
   end;

end Test;
