
--  Based on ARM 2022
--  $3.1:
--  basic_declaration ::= 
--       type_declaration | subtype_declaration
--     | object_declaration | number_declaration
--     | subprogram_declaration | abstract_subprogram_declaration
--     | null_procedure_declaration | expression_function_declaration
--     | package_declaration | renaming_declaration
--     | exception_declaration | generic_declaration
--     | generic_instantiation
--  $3.3.1: 
--  object_declaration ::= 
--        defining_identifier_list : [aliased] [constant] subtype_indication [:= expression]
--         [aspect_specification];
--      | defining_identifier_list : [aliased] [constant] access_definition [:= expression]
--         [aspect_specification];
--      | defining_identifier_list : [aliased] [constant] array_type_definition [:= expression]
--         [aspect_specification];
--      | single_task_declaration
--      | single_protected_declaration

--  This testcase concerns object declarations except single_task_declaration and 
--  single_protected_declaration.

with Ada.Numerics; use Ada.Numerics;

package Object_Declarations is
   
   -----------------------------------
   --  Multiple object declaration  --
   -----------------------------------
   
   type Person(<>);
   type Car is tagged; -- incomplete type declaration
   type Person_Name is access Person;
   type Car_Name is access all Car'Class;
   
   type Car is tagged record
      Number  : Integer;
      Owner   : Person_Name;
   end record;
   
   type Person(Sex : Gender) is record
      Name     : String(1 .. 20);
      Birth    : Date;
      Age      : Integer range 0 .. 130;
      Vehicle  : Car_Name;
      case Sex is
         when M => Wife           : Person_Name(Sex => F);
         when F => Husband        : Person_Name(Sex => M);
      end case;
   end record;
   subtype Male is Person(Sex => M);
   
   John, Paul : not null Person_Name := new Person(Sex => M);
   Max : not null Person_Name := new Person(Sex => M);
   
   -----------------------------
   --  Variable declarations  --
   -----------------------------
   
   Count, Sum  : Integer;
   Size        : Integer range 0 .. 10_000 := 0;
   Sorted      : Boolean := False;
   
   
   type Color  is (White, Red, Yellow, Green, Blue, Brown, Black);
   Color_Table : array(1 .. Max) of Color;
   
   type Bit_Vector is array(Integer  range <>) of Boolean;
   Option      : Bit_Vector(1 .. 10) := (others => True);
   
   Hello       : aliased String := "Hi, world.";
   Teta, Phi   : Float range -Pi .. +Pi;
   
   -----------------------------   
   --  Constant declarations  --
   -----------------------------
   
   Limit     : constant Integer := 10_000;
   Low_Limit : constant Integer := Limit/10;
   Tolerance : constant Real := Dispersion(1.15);
   A_String  : constant String := "A";
   Hello_Msg : constant access String := Hello'Access;
   
end Object_Declarations;
