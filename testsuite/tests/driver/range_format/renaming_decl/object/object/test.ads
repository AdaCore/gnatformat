
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
--  $8.5.1:
--  object_renaming_declaration ::= 
--     defining_identifier [: [null_exclusion] subtype_mark] renames object_name
--         [aspect_specification];
--   | defining_identifier : access_definition renames object_name
--     [aspect_specification];
  
--  This testcase concerns object renaming declarations.

with Ada.IO_Exceptions;

package Object_Renaming_Declaration is
   
   type Cell;
   type Link is access Cell;
   
   type Cell is record
      Value  : Integer;
      Succ   : Link;
      Pred   : Link;
   end record;
   
   Head   : Link  := new Cell'(0, null, null);
   Next   : Link  := Head.Succ;
   
   Renamed_Head : Link renames Head;
   
   One_Cell : Cell := Cell'(Value => 1, Succ => null, Pred => null);
   Same_Cell : Cell renames One_Cell;
   
   One : constant := 1;
   --  Uno renames One;
   
   type Acc_I is access Integer;
   subtype Acc_NN_I is not null Acc_I;
   Obj : Acc_I := null;

   B : Acc_NN_I;
   D : not null Acc_I renames B;
   
end Object_Renaming_Declaration;
