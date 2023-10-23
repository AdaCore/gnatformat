with Ada.Text_IO;
with Gnatfmt.Command_Line;

procedure Gnatfmt.Ada_Driver is
begin
   GNATCOLL.Traces.Parse_Config_File;

   if Gnatfmt.Command_Line.Parser.Parse then
      if Gnatfmt.Command_Line.Help.Get then
         Ada.Text_IO.Put_Line (Gnatfmt.Command_Line.Parser.Help);
      else
         if Gnatfmt.Command_Line.Verbose.Get then
            Gnatfmt_Trace.Set_Active (True);
         end if;
         --  !!!! TO DO: add a call to the entry point
      end if;
   end if;

end Gnatfmt.Ada_Driver;
