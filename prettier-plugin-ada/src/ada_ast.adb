--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Ada.Command_Line;
with GNATCOLL.JSON;
with Langkit_Support.Text;

-------------
-- Ada_Ast --
-------------

procedure Ada_Ast is

   function Read_From_Stdin return Unbounded_String;
   --  TODO

   ----------------------
   --  Read_From_Stdin --
   ----------------------

   function Read_From_Stdin return Unbounded_String  is
      Input     : Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
      Item      : Character;

      use Ada.Strings.Unbounded;
   begin
      while not Ada.Text_IO.End_Of_File loop
         Ada.Text_IO.Get_Immediate (Item);
         Append (Input, Item);
      end loop;

      --  Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Input));

      return Input;
   end Read_From_Stdin;

   Unit : constant Libadalang.Analysis.Analysis_Unit :=
     Libadalang.Analysis.Create_Context.Get_From_Buffer
       (Filename => "",
        Buffer   => Read_From_Stdin,
        Rule     => Grammar_Rule'Value (Ada.Command_Line.Argument (1)));

   function Process_Node
     (Node : Ada_Node)
      return GNATCOLL.JSON.JSON_Value;

   function Process_Node
     (Node : Ada_Node)
      return GNATCOLL.JSON.JSON_Value
   is
      Result : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create_Object;

   begin
      if Node.Is_Null then
         return GNATCOLL.JSON.JSON_Null;
      end if;
      GNATCOLL.JSON.Set_Field (Result, "kind", Node.Kind_Name);
      case Node.Kind is
         when Ada_Dotted_Name =>
            GNATCOLL.JSON.Set_Field
              (Result, "suffix",
               Process_Node (Node.As_Dotted_Name.F_Suffix.As_Ada_Node));
            GNATCOLL.JSON.Set_Field
              (Result, "prefix",
               Process_Node (Node.As_Dotted_Name.F_Prefix.As_Ada_Node));
         when Ada_Subp_Decl =>
            case Node.As_Subp_Decl.F_Overriding is
               when Ada_Overriding_Overriding =>
                  GNATCOLL.JSON.Set_Field (Result, "overriding", "overriding");
               when Ada_Overriding_Not_Overriding =>
                  GNATCOLL.JSON.Set_Field
                    (Result, "overriding", "not overriding");
               when Ada_Overriding_Unspecified =>
                  null;
            end case;
            GNATCOLL.JSON.Set_Field
              (Result, "spec",
               Process_Node (Node.As_Subp_Decl.F_Subp_Spec.As_Ada_Node));

         when Ada_Subp_Spec =>
            case Node.As_Subp_Spec.F_Subp_Kind is
               when Ada_Subp_Kind_Function =>
                  GNATCOLL.JSON.Set_Field
                    (Result, "subpKind", "function");
                  GNATCOLL.JSON.Set_Field
                    (Result,
                     "returns",
                    Langkit_Support.Text.To_UTF8
                       (Node.As_Subp_Spec.F_Subp_Returns.Text));

               when Ada_Subp_Kind_Procedure =>
                  GNATCOLL.JSON.Set_Field
                    (Result, "subpKind", "procedure");
            end case;
            GNATCOLL.JSON.Set_Field
              (Result,
               "name",
               Langkit_Support.Text.To_UTF8
                 (Node.As_Subp_Spec.F_Subp_Name.F_Name.Text));
            GNATCOLL.JSON.Set_Field
              (Result,
               "parameters",
               Process_Node
                 (Node.As_Subp_Spec.F_Subp_Params.F_Params.As_Ada_Node));

         when Ada_Param_Spec_List =>
            for Child_Or_Trivia of Node.Children_And_Trivia loop
               case Child_Or_Trivia.Kind is
               when Child =>
                  null;
                  --  Ada.Text_IO.Put_Line (Child_Or_Trivia.Node.Image);
               when Trivia =>
                  null;
                  --  Ada.Text_IO.Put_Line (Image (Child_Or_Trivia.Trivia));
               end case;
            end loop;
            declare
               Params : GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array);
            begin
               for Param_Spec of Node.As_Param_Spec_List loop
                  GNATCOLL.JSON.Append
                    (Params, Process_Node (Param_Spec.As_Ada_Node));
               end loop;
               return Params;
            end;

         when Ada_Param_Spec =>
            GNATCOLL.JSON.Set_Field
              (Val => Result, Field_Name => "ids",
               Field =>
                  Langkit_Support.Text.To_UTF8
                    (Node.As_Param_Spec.F_Ids.Text));
            case Node.As_Param_Spec.F_Mode is
               when Ada_Mode_Default =>
                  null;
               when Ada_Mode_In =>
                  GNATCOLL.JSON.Set_Field (Result, "mode", "in");
               when Ada_Mode_In_Out =>
                  GNATCOLL.JSON.Set_Field (Result, "mode", "in out");
               when Ada_Mode_Out =>
                  GNATCOLL.JSON.Set_Field (Result, "mode", "out");
            end case;
            if Node.As_Param_Spec.F_Has_Aliased then
               GNATCOLL.JSON.Set_Field (Result, "aliased", True);
            end if;
            GNATCOLL.JSON.Set_Field
              (Result, "type",
               Langkit_Support.Text.To_UTF8
                 (Node.As_Param_Spec.F_Type_Expr.Text));
            if not Node.As_Param_Spec.F_Default_Expr.Is_Null then
               GNATCOLL.JSON.Set_Field
                 (Result, "default_expr",
                  Langkit_Support.Text.To_UTF8
                    (Node.As_Param_Spec.F_Default_Expr.Text));
            end if;
            if not Node.As_Param_Spec.F_Aspects.Is_Null then
               GNATCOLL.JSON.Set_Field
                 (Result,
                  "aspects",
                  Langkit_Support.Text.To_UTF8
                    (Node.As_Param_Spec.F_Aspects.Text));
            end if;

         when others =>
            if Node.Children_Count > 0 then
               declare
                  Children : GNATCOLL.JSON.JSON_Value :=
                    GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array);
               begin
                  for Child of Node.Children loop
                     declare
                        Child_Value : constant GNATCOLL.JSON.JSON_Value :=
                          Process_Node (Child);
                        use type GNATCOLL.JSON.JSON_Value;
                     begin
                        if Child_Value /= GNATCOLL.JSON.JSON_Null then
                           GNATCOLL.JSON.Append (Children, Child_Value);
                        end if;
                     end;
                  end loop;
                  declare
                     Children_Array : constant GNATCOLL.JSON.JSON_Array :=
                       GNATCOLL.JSON.Get (Children);
                  begin
                     GNATCOLL.JSON.Set_Field
                       (Result, "children", Children_Array);
                  end;
               end;
            elsif Node.Kind in Ada_Identifier then
               GNATCOLL.JSON.Set_Field
                 (Result,
                  "value",
                  Langkit_Support.Text.To_UTF8 (Node.As_Identifier.Text));
            end if;
      end case;

      return Result;
   end Process_Node;

   Ast_Json : GNATCOLL.JSON.JSON_Value := Process_Node (Unit.Root);

begin
   Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Ast_Json, False));
end Ada_Ast;
