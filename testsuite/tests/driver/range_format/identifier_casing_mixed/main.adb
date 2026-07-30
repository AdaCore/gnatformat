procedure Test is
   MY_VAR : Integer := 0;
begin
   my_var := 1;
   declare
      local_var : Integer := 0;
   begin
      LOCAL_VAR := my_var + local_var;
   end;
end Test;
