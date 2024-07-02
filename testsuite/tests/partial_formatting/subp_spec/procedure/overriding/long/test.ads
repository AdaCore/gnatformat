package Test is
   type Foo is interface;
      
   procedure Bar (Self : Foo; I : Integer; I : Integer; J : Integer; K, L : Integer) is abstract;

   type Baz is new Foo with null record;

   overriding procedure Bar (Self : Baz; I : Integer; J : Integer; K, L,M,O,P : Integer);
   
   procedure Bar (Self : Foo; I : Integer);
end Test;
