
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
--  This testcase concerns subtype declarations.
--  $9.4:
--  single_protected_declaration ::= 
--      protected defining_identifier
--        [aspect_specification] is
--      [new interface_list with]
--      protected_definition;

--  This test file concerns single protected object declaration

with Ada.Text_IO; use Ada.Text_IO;

procedure Single_Protected_Decl is
   
   protected Obj is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Local : Integer := 0;
   end Obj;

   protected body Obj is
      --  procedures can modify the data
      procedure Set (V : Integer) is
      begin
         Local := V;
      end Set;

      --  functions cannot modify the data
      function Get return Integer is
      begin
         return Local;
      end Get;
   end Obj;
   
   protected Shared_Array is
      function  Component    (N : in Index) return Item;
      procedure Set_Component(N : in Index; E : in  Item);
   private
      Table : Item_Array(Index) := (others => Null_Item);
   end Shared_Array;
   
   protected body Shared_Array is
      function Component(N : in Index) return Item is
      begin
        return Table(N);
      end Component;
      
      procedure Set_Component(N : in Index; E : in Item) is
      begin
        Table(N) := E;
      end Set_Component;
   end Shared_Array;
   
   
begin
   Obj.Set (5);
   Put_Line ("Number is: "
             & Integer'Image (Obj.Get));
end Single_Protected_Decl;
