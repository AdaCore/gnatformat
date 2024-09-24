
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
--  $6.1:
--  subprogram_declaration ::= 
--    [overriding_indicator]
--    subprogram_specification
--        [aspect_specification];

--  This testcase concerns subprogram declarations.


package Subprogram_Declaration is
   
   procedure Traverse_Tree;
   procedure Increment(X : in out Integer);
   procedure Right_Indent(Margin : out Line_Size);          
   procedure Switch(From, To : in out Link);                

   function Random return Probability;                      

   function Min_Cell(X : Link) return Cell;                 
   function Next_Frame(K : Positive) return Frame;          
   function Dot_Product(Left, Right : Vector) return Real;  
   function Find(B : aliased in out Barrel; Key : String) return Real;
                                                         
   function "*"(Left, Right : Matrix) return Matrix;
   
   --  Subprogram having parameters with default expressions: 
   procedure Print_Header(Pages  : in Natural;
            Header : in Line    :=  (1 .. Line'Last => ' ');
            Center : in Boolean := True);
   
end Subprogram_Declaration;
