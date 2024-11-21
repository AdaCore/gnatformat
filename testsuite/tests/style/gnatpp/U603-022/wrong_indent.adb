
package body Wrong_Indent is

   procedure P1 is 
   begin
      Assert_Historical_Prices
	("xrates_in_back_check_interval_2",
	 Prices_To_JSON 
	    (T (Now,
	        S (Syd1,
		   No_Prices (Optimistic) &
		   No_Prices (Pessimistic)))));
      
      null;
   end P1; 
   
   procedure P2 is
   begin
      
   DB := Append
     (Speed'Access,
      Append
        (Str1'Access,
         Append
           (Str2'Access,
            Append
              (Str3'Access,
               Append (Str4'Access, Str5'Access)))));
   end P2;
   


end Wrong_Indent;
