package Gnatfmt is

   Version : constant String := "debug";
   
   type Symbol is private;
   
   function New_Symbol return Symbol;
   
private
   
   type Symbol is new Integer;

end Gnatfmt;
