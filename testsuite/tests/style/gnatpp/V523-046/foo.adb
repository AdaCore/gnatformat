package body Foo is

   -------
   -- F --
   -------

   function F (X : Integer) return Integer is (1);

   ------------------------------------
   -- Biiiiiiiiiiiiiiiiiiiiiiiiiig_F --
   ------------------------------------

   function Biiiiiiiiiiiiiiiiiiiiiiiiiig_F (X : Integer) return Integer is (1);

   function Another_Biiiiiiiiiiiiiiiiiiiiiiiiiig_F
   (X : Integer;
    Y : out Integer)
   return Integer is (1);

end Foo;
