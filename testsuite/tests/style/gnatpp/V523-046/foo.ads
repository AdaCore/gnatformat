package Foo is
   function F (X : Integer) return Integer with Import;
   -- An awesome function
   -- An awesome function.
   -- @param x One of its parameters.

   function Biiiiiiiiiiiiiiiiiiiiiiiiiig_F (X : Integer) return Integer with Import;
   -- An awesome function
   -- An awesome function.
   -- @param x One of its parameters.

   function Another_Biiiiiiiiiiiiiiiiiiiiiiiiiig_F
   (X : Integer;
    Y : out Integer)
   return Integer with
   -- Something about Pre
   Pre => X < 10,
   -- Something about Post
     Post => Y > X;
     -- An awesome function
     -- An awesome function.
     -- @param x One of its parameters.
end Foo;
