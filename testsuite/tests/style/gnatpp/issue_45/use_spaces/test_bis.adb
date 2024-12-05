package body Test is

   --------------------
   --  Get_My_Result --
   --------------------

   overriding function Get_My_Result (C : in My_Type) return My_Result is
      Result : World_Item_Id_List;
   begin
      if C.Nav_C_Id /= Null_World_Item_Id then
         Include (Result, C.Nav_C_Id);
      end if;

      --   my comment line 11111111111111111111111
      --  line 2222222222222222222222222222222222222222222

      -- my fillable comment xxxxxxxxxxxxxxxxxxxxxxxxxx
      -- my comment's 2nd line yyyyyyyyyyyyyyyyyyy

      return Result;
   end Get_My_Result;
end Test;
