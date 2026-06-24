procedure Test is
   My_Var : Integer := 0;
begin
   my_var := 1;
   declare
      Local_Var : Integer := 0;
   begin
      local_var := my_var + local_var;
   end;
end Test;
