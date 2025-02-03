
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
--  $6.7:
--  null_procedure_declaration ::= 
--    [overriding_indicator]
--    procedure_specification is null
--       [aspect_specification];

--  This testcase concerns null procedure declarations.


package Null_Procedure_Declaration is
   
   procedure Simplify(Expr : in out Expression) is null;
   
end Null_Procedure_Declaration;
