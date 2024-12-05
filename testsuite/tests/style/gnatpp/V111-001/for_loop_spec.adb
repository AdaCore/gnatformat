procedure for_loop_spec is

   type Index is new Integer range 1..10;

begin
   for I in Index'Range loop
      null;
   end loop;

   for I in Index'Range when I > 3 loop
      null;
   end loop;

end for_loop_spec;
