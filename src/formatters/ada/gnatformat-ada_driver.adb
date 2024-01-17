with Ada.Text_IO;
with Gnatformat.Command_Line;

procedure Gnatformat.Ada_Driver is
begin
   GNATCOLL.Traces.Parse_Config_File;

   if Gnatformat.Command_Line.Parser.Parse then
      if Gnatformat.Command_Line.Help.Get then
         Ada.Text_IO.Put_Line (Gnatformat.Command_Line.Parser.Help);
      else
         if Gnatformat.Command_Line.Verbose.Get then
            Gnatformat_Trace.Set_Active (True);
         end if;
         --  !!!! TO DO: add a call to the entry point
      end if;
   end if;

end Gnatformat.Ada_Driver;
