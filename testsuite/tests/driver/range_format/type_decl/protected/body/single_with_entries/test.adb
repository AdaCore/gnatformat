
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

--  This test file concerns protected object entries

with Ada.Text_IO; use Ada.Text_IO;

procedure Protected_Objects_Entries is

   protected Obj is
      procedure Set (V : Integer);
      entry Get (V : out Integer);
   private
      Local  : Integer;
      Is_Set : Boolean := False;
   end Obj;

   protected body Obj is
      procedure Set (V : Integer) is
      begin
         Local := V;
         Is_Set := True;
      end Set;

      entry Get (V : out Integer)
        when Is_Set is
         --  Entry is blocked until the
         --  condition is true. The barrier
         --  is evaluated at call of entries
         --  and at exits of procedures and
         --  entries. The calling task sleeps
         --  until the barrier is released.
      begin
         V := Local;
         Is_Set := False;
      end Get;
   end Obj;

   N : Integer := 0;

   task T;

   task body T is
   begin
      Put_Line ("Task T will delay for 4 seconds...");
      delay 4.0;
      Put_Line ("Task T will set Obj...");
      Obj.Set (5);
      Put_Line ("Task T has just set Obj...");
   end T;
begin
   Put_Line ("Main application will get Obj...");
   Obj.Get (N);
   Put_Line ("Main application has just retrieved Obj...");
   Put_Line ("Number is: " & Integer'Image (N));

end Protected_Objects_Entries;
