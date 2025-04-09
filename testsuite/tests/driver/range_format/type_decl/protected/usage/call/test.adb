
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

--  This test file concerns protected_type_declaration

with Ada.Text_IO; use Ada.Text_IO;

procedure Protected_Object_Type_Decl
is

   protected type P_Obj_Type is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Local : Integer := 0;
   end P_Obj_Type;

   protected body P_Obj_Type is
      procedure Set (V : Integer) is
      begin
         Local := V;
      end Set;

      function Get return Integer is
      begin
         return Local;
      end Get;
   end P_Obj_Type;

   Obj : P_Obj_Type;
begin
   Obj.Set (5);
   Put_Line ("Number is: "
             & Integer'Image (Obj.Get));
end Protected_Object_Type_Decl;
