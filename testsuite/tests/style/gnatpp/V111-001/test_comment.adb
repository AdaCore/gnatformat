procedure test_comment is

   t_var : array (1..5) of boolean := (1 => True,
   -- whole line comment 1
   2 | 4 => False,
   -- whole line comment 2
   others => True);
   t_var2 : array (1..5) of boolean := (1 => True,
   2 | 4 => False,
   -- whole line comment 2
   others => True);

   procedure A (X : Integer; Y, Z: Boolean);

   procedure A (X : Integer; Y, Z: Boolean) is
   begin
   null;
   end A;

begin

      A (X => 1,
   --  whole line comment
      Y => False,
	 Z => True); 
      
   A (X => 1,
   --  whole line comment
      False,
      Z => True);   
   
   A (1,
   --  whole line comment
      Y => False,
      Z => True);
   
     A (1, False,
     --  whole line comment
	Z => True);
     
           A (X => 1, --  EOL comment
      Y => False,
	 Z => True); 
   
end test_comment;
