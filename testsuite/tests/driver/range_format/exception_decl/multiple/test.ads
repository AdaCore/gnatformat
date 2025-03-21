
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
--  $11.1:
--  exception_declaration ::= defining_identifier_list : exception
--    [aspect_specification];

--  This testcase concerns exception declarations.


package Exception_Declaration is

Singular : exception;
Error    : exception;
Overflow, Underflow : exception;

end Exception_Declaration;
