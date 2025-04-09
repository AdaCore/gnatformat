
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
--  $7.1:
--  package_declaration ::= package_specification;
--  package_specification ::= 
--    package defining_program_unit_name
--        [aspect_specification] is
--      {basic_declarative_item}
--   [private
--      {basic_declarative_item}]
--    end [[parent_unit_name.]identifier]
	
--  This testcase concerns package declarations.


package Package_Declaration is
   
   type Rational is
      record
         Numerator   : Integer;
         Denominator : Positive;
      end record;

   function "="(X,Y : Rational) return Boolean;
   function "/"  (X,Y : Integer)  return Rational;  
   function "+"  (X,Y : Rational) return Rational;
   function "-"  (X,Y : Rational) return Rational;
   function "*"  (X,Y : Rational) return Rational;
   
end Package_Declaration;
