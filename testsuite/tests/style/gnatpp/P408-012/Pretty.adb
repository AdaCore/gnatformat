procedure Pretty is

    package IP is
      

      type Horizontal_Enum_Type is
        (Red, Blue, Green);  

      type Short_Enum_Type is (Red, 
                               Blue, 
                               Green);

      type Short_List_Enum_Type is
      (Red,
       Blue1,
       Blue2,
       Blue3,
       Blue4,
       Blue5,
       Blue6,
       Blue7,
       Blue8,
       Blue9,
       Blue0,
       Blue11,
       Blue12,
       Blue13,
       Blue14,
       Blue15,
       Blue16,
       Blue17,
       Green);

      type Long_Enum_Type is
      (Valuexxxxxxxxxxxxxxxxxxxxxxxxx1,
       Valuexxxxxxxxxxxxxxxxxxxxxxxxx2,
       Valuexxxxxxxxxxxxxxxxxxxxxxxxx3);

    end IP;

  type Horizontal_Enum_Type is (Red,Blue,Green); 

  type Short_Enum_Type is (Red,
                           Blue,
                           Green);

  type Short_List_Enum_Type is (Red,
                                Blue1,
                                Blue2,
                                Blue3,
                                Blue4,
                                Blue5,
                                Blue6,
                                Blue7,
                                Blue8,
                                Blue9,
                                Blue0,
                                Blue11,
                                Blue12,
                                Blue13,
                                Blue14,
                                Blue15,
                                Blue16,
                                Blue17,
                                Green);
   
  type Long_Enum_Type is (Valuexxxxxxxxxxxxxxxxxxxxxxxxx1,
                          Valuexxxxxxxxxxxxxxxxxxxxxxxxx2,
                          Valuexxxxxxxxxxxxxxxxxxxxxxxxx3);
                          
                          

                          
  Short_Enum_Lookup : constant array (1..3) of Short_Enum_Type := 
    (1 => Red,
     2 => Blue,
     3 => Green);

  Long_Enum_Lookup : constant array (1..3) of Long_Enum_Type := 
    (1 => Valuexxxxxxxxxxxxxxxxxxxxxxxxx1,
     2 => Valuexxxxxxxxxxxxxxxxxxxxxxxxx2,
     3 => Valuexxxxxxxxxxxxxxxxxxxxxxxxx3);

  Short_Ext_Lookup : constant array (1..3) of IP.Short_Enum_Type := 
    (1 => IP.Red,
     2 => IP.Blue,
     3 => IP.Green);

  Long_Ext_Lookup : constant array (1..3) of IP.Long_Enum_Type := 
    (1 => IP.Valuexxxxxxxxxxxxxxxxxxxxxxxxx1,
     2 => IP.Valuexxxxxxxxxxxxxxxxxxxxxxxxx2,
     3 => IP.Valuexxxxxxxxxxxxxxxxxxxxxxxxx3);
  
  
begin

  null;

end Pretty;
