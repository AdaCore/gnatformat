
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
--  $8.5:
--  renaming_declaration ::= 
--     object_renaming_declaration
--   | exception_renaming_declaration
--   | package_renaming_declaration
--   | subprogram_renaming_declaration
--   | generic_renaming_declaration
--  $8.5.3:
--  package_renaming_declaration ::= 
--    package defining_program_unit_name renames package_name
--    [aspect_specification];
    
--  This testcase concerns package renaming declarations

package Package_Renaming_Declaration renames Renaming_Declaration;
