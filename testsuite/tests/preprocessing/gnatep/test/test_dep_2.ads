package Test_Dep_2 is
   #if DEBUG then
   A : constant Integer := 1;
   #else
   B : constant Integer := 2;
   #end if;
end Test_Dep_2;