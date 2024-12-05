
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
--  $3.3.1:
--  object_declaration ::= 
--      defining_identifier_list : [aliased] [constant] subtype_indication [:= expression]
--          [aspect_specification];
--    | defining_identifier_list : [aliased] [constant] access_definition [:= expression]
--          [aspect_specification];
--    | defining_identifier_list : [aliased] [constant] array_type_definition [:= expression]
--          [aspect_specification];
--    | single_task_declaration
--    | single_protected_declaration
--  $9.1: single_task_declaration ::= 
--   task defining_identifier 
--        [aspect_specification] [is
--     [new interface_list with]
--     task_definition];

--  This testcase concerns simple task declaration in a package

with Ada.Text_IO; use Ada.Text_IO;

package body Single_Task_Decl_Pckg is

   task body T is
   begin
      for I in 1 .. 10 loop
         Put_Line ("hello");
      end loop;
   end T;   
   
end Single_Task_Decl_Pckg;
