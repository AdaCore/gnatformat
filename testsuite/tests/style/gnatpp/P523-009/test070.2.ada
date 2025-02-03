WITH Ada.Text_IO;
WITH test_gnatpp;

PROCEDURE test070 IS
    switch : test_gnatpp.development_switch := test_gnatpp.none;
BEGIN
    Ada.Text_IO.Put_Line ("Test070 Start");

    Ada.Text_IO.Put_Line ("  switch => " & test_gnatpp.development_switch'image (switch));

    Ada.Text_IO.Put_Line ("Test070 End");
END test070;
