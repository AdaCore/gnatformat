package body Foo is

   procedure Create
     (Name: in Ada.Strings.Unbounded.Unbounded_String;
      Config: in Bar;
      ID: out Bar)
   is
   begin
      ID := Get_ID;
      Nodes.Insert
        (Key => ID,
         New_Item =>
           Foo'
             (State => Pending,
              Name => Name,
              Config => Config,
              Foos => My_Set.Empty_Set,
              Bars => My_Set.Empty_Set,
              Bazs => My_Set.Empty_Set,
              X => 0));
      Register_Pending_Node(ID);
   end Create;

end Foo;
