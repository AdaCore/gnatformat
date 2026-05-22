package Test_Align is

    Suspension_Info             : constant Flight_Suspension.T := CASA_Result.Suspension;
    Flight_Status                  : constant Standard.Flight_Status.T := FTFX.The_Flight_Status;
    Slot_Issued_Status        : constant Boolean := CASA_Result.Slot_Issued;
    Discrepancy_Details       : constant CDM.Discrepancy_Details.T := CDM.Discrepancy_Details.Get;
    Reval_Errors                   : constant Flight_Plan_Errors.List.T := Normal_Flight.Reval_Errors;

    Suspension : constant Flight_Suspension.T := CASA_Result.Suspension;

    
end Test_Align;
