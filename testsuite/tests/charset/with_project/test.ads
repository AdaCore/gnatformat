package Test is
   type Indicator_Range is range 1 .. 4;
   Indicator_Nice : constant array (Indicator_Range) of Wide_Wide_Character := ('◴', '◷', '◶', '◵');
end Test;
