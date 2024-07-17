
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
--  $8.5.5:
--  generic_renaming_declaration ::= 
--     generic package defining_program_unit_name renames generic_package_name
--         [aspect_specification];
--   | generic procedure defining_program_unit_name renames generic_procedure_name
--         [aspect_specification];
--   | generic function defining_program_unit_name renames generic_function_name
--     [aspect_specification];
  
--  This testcase concerns generic renaming declarations

with Ada.Text_IO;
generic package Generic_Renaming_Declaration renames Ada.Text_IO.Enumeration_IO;
