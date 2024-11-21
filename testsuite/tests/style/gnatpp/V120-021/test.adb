procedure test is

type my_long_named_type_array is array (1..10) of Boolean;
type my_very_long_long_named_type_array is array (1..10) of my_long_named_type_array;

my_long_named_array_variable : constant my_long_named_type_array := (others => False);
my_very_long_long_named_array_aliased_variable : aliased my_very_long_long_named_type_array := (others => (others => False));
my_very_long_long_named_array_constant_variable : constant my_very_long_long_named_type_array := (others => (others => False));
my_very_long_long_named_array_simple_variable : my_very_long_long_named_type_array := (others => (others => False));
my_very_long_long_named_array_all_variable : aliased constant my_very_long_long_named_type_array := (others => (others => False));

my_very_long_named_anonymus_array_variable : array (1..10) of my_long_named_type_array;
my_very_long_named_anonymus_array_var : my_very_long_long_named_type_array;
my_very_long_named_anonymus_array_variable : constant array (1..10) of my_long_named_type_array := (others => (others => False));

my_very_long_named_anonymus_array_var_renames : my_very_long_long_named_type_array_ren renames my_very_long_long_named_type_array;


begin

my_very_long_named_anonymus_array_var_renames := (others => (others => False));


   

end test;