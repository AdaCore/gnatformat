with Ada_Package;

package Test is
   
   type Rec_1 is record
      Int_Data : Integer;
   end record;
   for Rec_1_Type use record
      Int_Data at 0 range 0 .. 31;
      pragma Warnings (Off);
   end record;
   
   type Rec_2 (Val : Integer := 0) is record
      Val : String (1 .. 10);   
   end record;
   
   type Rec_3 (Val : Boolean := False) is record
      case Val is
	 when False => null;
	 when True => 
	    Data : Boolean;
      end case;      
   end record;
     
   
TYPE REC_4(NB:LARGE_CHARACTER_NB_TYPE:=LARGE_CHARACTER_NB_TYPE'FIRST)IS
RECORD
DATA:STRING(POSITIVE(CHARACTER_NB_TYPE'FIRST)..NB);
END RECORD;

TYPE REC_5(
FIELD:PROTOCOL_FIELD_ENUMERATED_TYPE:=DATA_LENGTH)IS
RECORD
CASE FIELD IS
WHEN DATA_LENGTH=>
NULL;
WHEN PROTOCOL_TYPE=>
VALUE:INTEGER;
END CASE;
END RECORD;
      
end Test;
