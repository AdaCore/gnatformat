procedure Compact_Array_Type_Declaration
is

   package Package_1 is
      package Sub_Package_Aa1 is
         type Very_Very_Long_Identifier_To_Annoy_Gnatpp_Very_Very_Much is range 0 .. 10;

         type Another_Long_And_Annoying_Identifier is range 0 .. 10;

         type Still_A_Long_And_Annoying_Identifier is range 0 .. 10;
      end Sub_Package_Aa1;
   end Package_1;

   type Array_1_Type is
      array (Integer range <>)
      of Float;

   type Array_2_Type is
      array (Package_1.Sub_Package_Aa1.Very_Very_Long_Identifier_To_Annoy_Gnatpp_Very_Very_Much,
             Package_1.Sub_Package_Aa1.Another_Long_And_Annoying_Identifier,
             Package_1.Sub_Package_Aa1.Still_A_Long_And_Annoying_Identifier)
      of Boolean;

   type Array_3_Type is
      array (Package_1.Sub_Package_Aa1.Very_Very_Long_Identifier_To_Annoy_Gnatpp_Very_Very_Much range <>,
             Package_1.Sub_Package_Aa1.Another_Long_And_Annoying_Identifier range <>,
             Package_1.Sub_Package_Aa1.Still_A_Long_And_Annoying_Identifier range <>)
      of Boolean;

begin
   null;
end Compact_Array_Type_Declaration;
