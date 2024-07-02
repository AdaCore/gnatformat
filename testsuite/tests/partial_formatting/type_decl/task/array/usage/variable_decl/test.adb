
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
--  $3.2.1:
--  type_declaration ::=  full_type_declaration
--     | incomplete_type_declaration
--     | private_type_declaration
--     | private_extension_declaration
--  $3.2.1:
--  full_type_declaration ::= 
--       type defining_identifier [known_discriminant_part] is type_definition
--          [aspect_specification];
--     | task_type_declaration
--     | protected_type_declaration

--  This testcase concerns task type declaration.

with Ada.Text_IO; use Ada.Text_IO;

procedure Task_Type_Arr is
   
   task type TT is      
      entry Start (N : Integer);
   end TT;

   task body TT is
      Task_N : Integer;
   begin
      accept Start (N : Integer) do
         Task_N := N;
      end Start;
      Put_Line ("In task T: "
                & Integer'Image (Task_N));
   end TT;

   My_Tasks : array (1 .. 5) of TT;
begin
   Put_Line ("In main");

   for I in My_Tasks'Range loop
      My_Tasks (I).Start (I);
   end loop;
end Task_Type_Arr;
