with Ada.Strings.Unbounded;
with Libadalang.Common;
with Prettier_Ada.Documents.Builders;

package body Gnatfmt.Ada_Formatter is

   function Print_Dotted_Name_IR
     (Node : Libadalang.Analysis.Dotted_Name)
      return Prettier_Ada.Documents.Document_Type;
   --  TODO: Add description

   --------------
   -- Print_IR --
   --------------

   function Print_IR
     (Node : Libadalang.Analysis.Ada_Node)
      return Prettier_Ada.Documents.Document_Type
   is
   begin
      case Node.Kind is
         when Libadalang.Common.Ada_Dotted_Name =>
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
      return Prettier_Ada.Documents.Document_Type
   is
      pragma Unreferenced (Node);
   begin
      return Prettier_Ada.Documents.Builders.Text
        (Ada.Strings.Unbounded.To_Unbounded_String ("TODO"));
   end Print_Dotted_Name_IR;

end Gnatfmt.Ada_Formatter;