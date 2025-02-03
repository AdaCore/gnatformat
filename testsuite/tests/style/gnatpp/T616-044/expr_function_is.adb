package body expr_function_is is
   
      
   function F0 (A   : Integer)
	       return Boolean is null;
	       
   function F1 (A   : Integer;
                B   : Integer)
	       return Boolean is (A >= 0 and B >=0 and A <=B);

   function F2 (X : Natural)
	       return Integer is (Integer (X));  
   
   function F3 (X : Natural) return String is ("(" & Trim (X'Img, Both) & " x )");
     
   function F4 (X : Natural) return Integer is 
      Res : Integer;
   begin
      Res := Integer (X);
      return Res;  
   end F4;
   
end expr_function_is;

