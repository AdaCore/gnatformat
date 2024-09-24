package Test is
   type Foo is interface;

   procedure Bar (Self : Foo; I : Integer; J : Integer; K, L : Integer) is abstract;

   type Baz is new Foo with null record;

   not overriding procedure Bar (Self : Baz; I : Integer; J : Integer; K, L : Integer);

end Test;
