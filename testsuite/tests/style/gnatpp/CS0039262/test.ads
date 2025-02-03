
package test is

procedure P1
    (V1 : in String;
    V2 : in Enum := Val;
     V3 : in Boolean := False)
    renames P;

procedure P2
    (V1 : in String;
    V2 : in Enum := Val;
     V3 : in Boolean)
    renames P1;


procedure P
    (V1 : in String;
    V2 : in Enum;
     V3 : in Boolean := False)
    renames P1;

  type Bits_T is
      record
      Lower : Tds.Bit_Index_T;
      Upper : Tds.Bit_Index_T;
  end record;


end test;