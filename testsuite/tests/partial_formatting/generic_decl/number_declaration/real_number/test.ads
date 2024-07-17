
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
--  $3.3.2: 
--  number_declaration ::= 
--       defining_identifier_list : constant := static_expression;

--  This testcase concerns number declarations.

with Ada.Numerics;

package Number_Declaration is
   
   --  Real number declaration
   Two_Pi        : constant := 2.0*Ada.Numerics.Pi;
   
   --  Integer number declaration 
   Max           : constant := 500;     -- an integer number
   Max_Line_Size : constant := Max/6;   -- the integer 83
   Power_16      : constant := 2**16;   -- the integer 65_536
   One, Un, Eins : constant := 1;       -- 3 different names for 1
      
end Number_Declaration;
