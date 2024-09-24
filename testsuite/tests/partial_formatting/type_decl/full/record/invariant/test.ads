
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

--  This testcase concerns type declarations; in addition, task_type_declaration
--  and protected_type_declaration will be handled also separately.

with Ada.Finalization;

package Type_Declarations is

   ------------------------------
   --  Full type declarations  -- 
   ------------------------------

   type Color  is (White, Red, Yellow, Green, Blue, Brown, Black);
   type Column is range 1 .. 72;   
   type Table  is array(1 .. 10) of Integer; 

   type My_Int is range 0 .. 1000;
   type Index is range 1 .. 5;

   --  My before comment
   type My_Int_Array is
     array (Index) of My_Int;
   --                 ^ Type of elements
   --       ^ Bounds of the array

   Arr : My_Int_Array := (2, 3, 5, 7, 11);
   --                    ^ Array literal
   --                      (aggregate)
   V : My_Int;

   -----------------------------------
   --  Incomplete type declaration  --
   -----------------------------------

   --  Recursive type example
   type Cell;
   type Link is access Cell;

   type Cell is record
       Value  : Integer;
       Succ   : Link;
       Pred   : Link;
   end record;

   Head   : Link  := new Cell'(0, null, null);
   Next   : Link  := Head.Succ;


   --  Mutually dependent access types examples

   type Person(<>);
   type Car is tagged;

   type Person_Name is access Person;
   type Car_Name    is access all Car'Class;

   type Car is tagged record
      Number  : Integer;
      Owner   : Person_Name;
   end record;

   type Person(Sex : Gender) is record
      Name     : String(1 .. 20);
      Birth    : Date;
      Age      : Integer range 0 .. 130;
      Vehicle  : Car_Name;
      case Sex is
         when M => Wife           : Person_Name(Sex => F);
         when F => Husband        : Person_Name(Sex => M);
      end case;
   end record;

   My_Car, Your_Car, Next_Car : Car_Name := new Car;
   Someone : Person_Name := new Person(M);


   --------------------------------- 
   --  Private type declarations  --
   ---------------------------------
   type Key is private;   
   type File_Name is limited private;

   -------------------------------------
   --  Private extension declaration  --
   -------------------------------------

   type List is new Ada.Finalization.Controlled with private;

   ------------------------------
   --  Task types declarations --
   ------------------------------

   task type Server is
      entry Next_Work_Item(WI : in Work_Item);
      entry Shut_Down;
   end Server;
   task type Keyboard_Driver(ID : Keyboard_ID := New_ID) is
      new Serial_Device with
     entry Read (C : out Character);
   entry Write(C : in  Character);
   end Keyboard_Driver;

   ---------------------------------
   --  Single tasks declarations  --
   ---------------------------------

   task Controller is
      entry Request(Level)(D : Item);  --  a family of entries
   end Controller;

   task Parser is
      entry Next_Lexeme(L : in  Lexical_Element);
      entry Next_Action(A : out Parser_Action);
   end;

   task User;

   --  Task objects examples 
   Agent    : Server;
   Teletype : Keyboard_Driver(TTY_ID);
   Pool     : array(1 .. 10) of Keyboard_Driver;

   --  Access type designating task objects example 
   type Keyboard is access Keyboard_Driver;
   Terminal : Keyboard := new Keyboard_Driver(Term_ID);

end Type_Declarations;
