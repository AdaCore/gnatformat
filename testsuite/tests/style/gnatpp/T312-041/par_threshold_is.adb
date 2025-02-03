package body par_threshold_is is
   
   procedure P0 is null;
   
   procedure P1 is 
   begin
      null;
   end P1;
   
   procedure P2 (A   : Integer; B   : Integer) is
      C : Integer;
   begin
      C := A + B;
   end P2;
      
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
   
end par_threshold_is;

