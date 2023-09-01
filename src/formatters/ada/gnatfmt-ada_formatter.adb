with Gnatfmt.Documents;
with Gnatfmt.Documents.Builders;

package body Gnatfmt.Ada_Formatter is

   --------------
   -- Print_IR --
   --------------

   function Print_IR
     (Node : Libadalang.Analysis.Ada_Node)
      return Gnatfmt.Documents.Document_Type
   is
   begin
      case Node.Kind is
         when Ada_Dotted_Name =>
            return Print_Dotted_Name_IR (Node.As_Dotted_Name);

         when others =>
            raise Program_Error;
      end case;
   end Print_IR;

   --------------------------
   -- Print_Dotted_Name_IR --
   --------------------------

   function Print_Dotted_Name_IR
     (Node : Libadalang.Analysis.Dotted_Name)
      return Gnatfmt.Documents.Document_Type
   is
   begin
      return Gnatfmt.Documents.Builders.Text ("TODO");
   end Print_Dotted_Name_IR;

end Gnatfmt.Ada_Formatter;