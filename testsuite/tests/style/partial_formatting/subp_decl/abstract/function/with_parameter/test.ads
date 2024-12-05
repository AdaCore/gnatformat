
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
--  $3.9.3:
--  abstract_subprogram_declaration ::= 
--    [overriding_indicator]
--    subprogram_specification is abstract
--        [aspect_specification];

--  This testcase concerns abstract subprogram declarations.

package Abstract_Subprogram_Declaration is
   
    subtype Element_Type is Natural;
    type Set is abstract tagged null record;
    
    function Empty return Set is abstract;
    function Union(Left, Right : Set) return Set is abstract;
    function Intersection(Left, Right : Set) return Set is abstract;
    function Unit_Set(Element : Element_Type) return Set is abstract;
    procedure Take(Element : out Element_Type;
                   From : in out Set) is abstract;
   
end Abstract_Subprogram_Declaration;
