package Test is

   procedure Test_P
     (V1 : in out My_Type;
      V2 : in out My_Other_Type);
	
	function Test_F (Item : in Object) return String;
	
   overriding procedure Test_P1
     (V1 : in out My_Type;
      V2 : in out My_Other_Type);
	
	overriding function Test_F1 (Item : in Object) return String;
	
   not overriding procedure Test_P2
     (V1 : in out My_Type;
      V2 : in out My_Other_Type);
	not overriding function Test_F2 (Item : in Object) return String;

end Test;
