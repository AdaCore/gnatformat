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
--  $12.3:
--  generic_instantiation ::= 
--     package defining_program_unit_name is
--         new generic_package_name [generic_actual_part]
--            [aspect_specification];
--    | [overriding_indicator]
      
--  This testcase concerns generic instantiation.

with Generic_Subprogram_Declaration;
with Generic_Package_Declaration;

package Generic_Instantiation is
   
   procedure Swap is new Exchange(Integer);
   procedure Swap is new Exchange(Character);
   function Square is new Squaring(Integer);
   
   package Int_Vectors is new Generic_Package_Declaration (Integer, Table, "+");
   
end Generic_Instantiation;
