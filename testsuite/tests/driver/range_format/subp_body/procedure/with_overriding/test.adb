package body Test is
   overriding
   procedure Bar
     (Self : Baz;
      I    : Integer)
   is
   begin
      null;
   end Bar;
end Test;
