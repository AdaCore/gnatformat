package Test is
   type Foo is interface;

   procedure Bar (Self : Foo; I : Integer) is abstract;

   type Baz is new Foo with null record;

   overriding procedure Bar (Self : Baz; I : Integer);

end Test;
