procedure Test with Spark_Mode is
   package Nested is

   function Invariant return Boolean;
private
pragma Style_Checks (Off);

#if SPARK_BODY_MODE="Off"
   pragma SPARK_Mode (Off);
#end if;

pragma Style_Checks (On);

   function Invariant return Boolean is (True);
end Nested;
begin
null;
end;
