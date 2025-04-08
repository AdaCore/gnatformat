
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
--  $12.1:
--  generic_declaration ::= 
--    generic_subprogram_declaration | generic_package_declaration
--  generic_package_declaration ::= 
--    generic_formal_part  package_specification;
    
--  This testcase concerns generic package declarations.

generic
   type Item   is private;
   type Vector is array (Positive range <>) of Item;
   with function Sum(X, Y : Item) return Item;
   
package Generic_Package_Declaration is
   
   function Sum  (A, B : Vector) return Vector;
   function Sigma(A    : Vector) return Item;
   Length_Error : exception;
   
end Generic_Package_Declaration;
