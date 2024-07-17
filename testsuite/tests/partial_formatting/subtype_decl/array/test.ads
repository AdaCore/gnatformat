
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
--  $3.2.2:
--  subtype_declaration ::= 
--     subtype defining_identifier is subtype_indication
--        [aspect_specification];

--  This testcase concerns subtype declarations.

with Ada.Finalization;

package Subtype_Declarations is
      
   type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
   subtype Rainbow is Color range Red .. Blue;
   subtype Red_Blue is Rainbow;
   
   ------------------------
   subtype Int is Integer;   
   subtype Small_Int is Integer range -10 .. 10;
   
   ------------------------
   type Column is range 1 .. 72;
   subtype Up_To_K is Column range 1 .. K;
   
   ------------------------   
   type Matrix is array(Integer  range <>, Integer range <>) of Real;
   subtype Square is Matrix(1 .. 10, 1 .. 10);
   
   ------------------------
   type Person(<>);
   type Car is tagged;
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
   
   ------------------------
   type Expression is tagged null record;
   type Expr_Ptr is access all Expression'Class;
   type Binary_Operation is new Expression with 
      record                 -- an internal node in an Expression tree
	 Left, Right : Expr_Ptr;	 
      end record;
   type Binop_Ptr is access all Binary_Operation'Class;
   subtype Binop_Ref is not null Binop_Ptr;
   
end Subtype_Declarations;
