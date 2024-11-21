with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Gnatpp_Array_Formatting is
  type Rec_T is record
    S : Unbounded_String;
    B : Boolean;
  end record;

  A_String           : Unbounded_String := To_Unbounded_String ("a string");
  Another_String     : Unbounded_String := To_Unbounded_String ("another string");
  Yet_Another_String : Unbounded_String := To_Unbounded_String ("yet another string");

  Arr : constant array (1 .. 3) of Rec_T :=
   ((A_String, True),
    (Another_String, False),
    (Yet_Another_String, False));

  A : constant Rec_T := Rec_T'(A_String, True);
  B : constant Rec_T := Rec_T'(Another_String, False);
  C : Rec_T          := Rec_T'(Yet_Another_String, False);

  pragma Unreferenced (Arr, A, B, C);
begin
  null;
end Gnatpp_Array_Formatting;
