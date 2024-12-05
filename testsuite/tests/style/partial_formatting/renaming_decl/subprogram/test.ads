
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
--  $8.5.4:
--  subprogram_renaming_declaration ::= 
--    [overriding_indicator]
--    subprogram_specification renames callable_entity_name
--        [aspect_specification];

--  This testcase concerns subprogam renaming declarations.

with Ada.IO_Exceptions;

package Subprogram_Renaming_Declaration is
   
   type T is tagged null record;
   function Predefined_Equal(X, Y : T) return Boolean renames "=";
   
private
   function "="(X, Y : T) return Boolean;
   
end Subprogram_Renaming_Declaration;
