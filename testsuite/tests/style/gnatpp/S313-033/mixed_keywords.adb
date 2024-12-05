procedure Mixed_Keywords is

   --  This test uses post-Ada-83 reserved words as identifiers and as reserved
   --  words. We are testing that gnatpp can handle all Ada language versions
   --  without knowing the version. This code is of course highly illegal in
   --  any Ada version, but we're trying to be tolerant.

   type Abstract is abstract tagged null record;
   procedure ABSTRACT;
   procedure abstract (X : Boolean);
   type Tagged is abstract tagged null record;
   procedure TAGGED;
   procedure tagged (X : Boolean);

   Aliased : aliased Integer;
   procedure ALIASED;
   procedure aliased (X : Boolean);

   protected type protected is
   end PROTECTED;
   procedure PROTECTED;
   procedure Protected (X : Boolean);

   procedure UNTIL;
   procedure Until (X : Boolean);
   procedure until (X : Integer);

   type Interface is Interface;
   procedure INTERFACE;
   procedure Interface (X : Boolean);
   procedure interface (X : Integer);

   OVERRIDING procedure OVERRIDING;
   Overriding procedure Overriding (X : Boolean);
   overriding procedure overriding (X : Integer);

   procedure SOME;
   procedure Some (X : Boolean);
   procedure some (X : Integer);

begin
   delay until Whenever;
end Mixed_Keywords;
