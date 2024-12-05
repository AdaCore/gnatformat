package body insert_blank_lines is
   
   -----------------------------------------------------------------------------

   type T_Limited_Controlled_Tagged_Type is new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Initialize (V_Limited_Controlled : in out T_Limited_Controlled_Tagged_Type);
   overriding procedure Finalize   (V_Limited_Controlled : in out T_Limited_Controlled_Tagged_Type);

   -----------------------------------------------------------------------------

   Global_Action_Handle : aliased Some_Tagged_Type;

   -----------------------------------------------------------------------------

   overriding procedure Initialize (V_Limited_Controlled : in out T_Limited_Controlled_Tagged_Type)
   is
      pragma Unreferenced (V_Limited_Controlled);
   begin
      Plugins.Action.Registry.Set_Global_Action (Handler => Global_Action_Handle'Access);
   end Initialize;

end insert_blank_lines;

