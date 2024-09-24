
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
--  $6.8:
--  expression_function_declaration ::= 
--      [overriding_indicator]
--      function_specification is
--        (expression)
--        [aspect_specification];
--    | [overriding_indicator]
--      function_specification is
--        aggregate
--        [aspect_specification];

--  This testcase concerns expression function declarations.


package Expression_Function_Declaration is
   
   type Point is tagged record
    X, Y : Real := 0.0;
   end record;
   
   function Is_Origin (P : in Point) return Boolean is
   (P.X = 0.0 and P.Y = 0.0);
   
end Expression_Function_Declaration;
