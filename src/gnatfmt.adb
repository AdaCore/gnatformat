package body Gnatfmt is
   Current_Symbol : Symbol := 0;
   
   ----------------
   -- New_Symbol --
   ----------------

   function New_Symbol return Symbol is
   begin
      return S : constant Symbol := Current_Symbol do
         Current_Symbol := Current_Symbol + 1;
      end return;
   end New_Symbol;

end Gnatfmt;