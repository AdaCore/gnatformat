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
--  generic_subprogram_declaration ::= 
--    generic_formal_part subprogram_specification 
--    [aspect_specification];
    
--  This testcase concerns generic subprogram declarations.

package Generic_Subprogram_Declaration is
   
   generic
      type Elem is private;
   procedure Exchange(U, V : in out Elem);
   
   generic
      type Item (<>) is private;
      with function "*"(U, V : Item) return Item is <>;
      
   function Squaring(X : Item) return Item;
   
end Generic_Subprogram_Declaration;
