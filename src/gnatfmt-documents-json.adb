with GNATCOLL.JSON;

package body Gnatfmt.Documents.Json is

   ---------------
   -- Serialize --
   ---------------

   function Serialize (Document : Document_Type) return String
   is
   begin
      raise Program_Error;
      return "";
   end Serialize;

   -----------------
   -- Deserialize --
   -----------------

   function Deserialize (Document : String) return Document_Type
   is
      Document_Id : Integer := Integer'First;

      Read_Result : constant GNATCOLL.JSON.Read_Result :=
        GNATCOLL.JSON.Read
          (Ada.Strings.Unbounded.To_Unbounded_String (Document));

      function To_Document_Type
        (Json : GNATCOLL.JSON.JSON_Value)
         return Document_Type;
      --  TODO: Description

      ----------------------
      -- To_Document_Type --
      ----------------------

      function To_Document_Type
        (Json : GNATCOLL.JSON.JSON_Value)
         return Document_Type
      is
         use GNATCOLL.JSON;

         function To_Document_Text
           (Json : GNATCOLL.JSON.JSON_Value)
            return Document_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Document_List
           (Json : GNATCOLL.JSON.JSON_Value)
            return Document_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Document_Command
           (Json : GNATCOLL.JSON.JSON_Value)
            return Document_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Align
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Break_Parent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Fill
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Group
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Indent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Indent_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Line
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Label
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Line_Suffix
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Line_Suffix_Boundary
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Trim
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         ----------------------
         -- To_Document_Text --
         ----------------------

         function To_Document_Text
           (Json : GNATCOLL.JSON.JSON_Value)
            return Document_Type
         is
         begin
            return
              Document : constant Document_Type :=
                Document_Type'
                  (Bare_Document =>
                     new Bare_Document_Type'
                           (Kind => Document_Text,
                            Id   => Document_Id,
                            Text => Get (Json)))
            do
               Document_Id := @ + 1;
            end return;
         end To_Document_Text;

         ----------------------
         -- To_Document_List --
         ----------------------

         function To_Document_List
           (Json : GNATCOLL.JSON.JSON_Value)
            return Document_Type
         is
            Elements  : constant JSON_Array := Get (Json);
            Length    : constant Natural :=
              GNATCOLL.JSON.Length (Elements);
            Documents : constant Document_Type_Array_Access :=
              new Document_Type_Array (1 .. Length);

         begin
            for J in 1 .. Length loop
               Documents (J) := To_Document_Type (Get (Elements, J));
            end loop;

            return
              Document : constant Document_Type :=
                Document_Type'
                  (Bare_Document =>
                     new Bare_Document_Type'
                           (Kind => Document_List,
                            Id   => Document_Id,
                            List => Documents))
            do
               Document_Id := @ + 1;
            end return;
         end To_Document_List;

         -------------------------
         -- To_Document_Command --
         -------------------------

         function To_Document_Command
           (Json : GNATCOLL.JSON.JSON_Value)
            return Document_Type
         is
            Command_Text : constant UTF8_String :=  Get (Json, "command");

         begin
            case Command_Text is
               when "align" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Align (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "breakParent" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Break_Parent (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "fill" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Fill (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "group" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Group (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "ifBreak" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_If_Break (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "indent" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Indent (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "indentIfBreak" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Indent_If_Break (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "line" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Line (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "label" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Label (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "lineSuffix" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Line_Suffix (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "lineSuffixBoundary" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'
                             (To_Command_Line_Suffix_Boundary (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when "trim" =>
                  declare
                     Kind    : constant Document_Kind_Type := Document_Command;
                     Id      : constant Integer := Document_Id;
                     Command : constant Command_Type_Access :=
                       new Command_Type'(To_Command_Trim (Json));
                  begin
                     return
                       Document : constant Document_Type :=
                         Document_Type'
                           (Bare_Document =>
                              new Bare_Document_Type'(Kind, Id, Command))
                     do
                        Document_Id := @ + 1;
                     end return;
                  end;

               when others =>
                  --  TODO: Raise a better exception
                  raise Program_Error;
            end case;
         end To_Document_Command;

         ----------------------
         -- To_Command_Align --
         ----------------------

         function To_Command_Align
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            function To_Align_Data
              (Json : GNATCOLL.JSON.JSON_Value)
               return Align_Data_Type;
            --  TODO: Add description

            -------------------
            -- To_Align_Data --
            -------------------

            function To_Align_Data
              (Json : GNATCOLL.JSON.JSON_Value)
               return Align_Data_Type
            is
            begin
               if Kind (Json) = JSON_Null_Type then
                  return Align_Data_Type'(Kind => None);
               else
                  declare
                     Command_Kind : constant String :=
                       Get
                         (Get (Get (Json, "command"), "alignData"), "kind");
                     Align_Data_Kind : constant Align_Kind_Type :=
                       (if Command_Kind = "dedentToRoot" then
                          Dedent_To_Root
                        elsif Command_Kind = "dedent" then
                          Dedent
                        elsif Command_Kind = "root" then
                          Root
                        elsif Command_Kind = "text" then
                          Text
                        elsif Command_Kind = "width" then
                          Width
                        else
                          raise Program_Error);
                     Align_Data : constant Align_Data_Type :=
                       (case Align_Data_Kind is
                          when Dedent_To_Root => (Kind => Dedent_To_Root),
                          when Dedent => (Kind => Dedent),
                          when Root => (Kind => Root),
                          when Text =>
                            (Kind => Text,
                             T    =>
                               Get
                                 (Get (Get (Json, "command"), "alignData"),
                                  "t")),
                          when Width =>
                            (Kind => Width,
                             N    =>
                               Get
                                 (Get (Get (Json, "command"), "alignData"),
                                  "n")),
                          when None => raise Program_Error);

                  begin
                     return Align_Data;
                  end;
               end if;
            end To_Align_Data;

            Kind           : constant Command_Kind_Type := Command_Align;
            Align_Data     : constant Align_Data_Type :=
              To_Align_Data (Get (Json, "alignData"));
            Align_Contents : constant Document_Type :=
              To_Document_Type (Get (Json, "alignContents"));

         begin
            return Command_Type'(Kind, Align_Data, Align_Contents);
         end To_Command_Align;

         -----------------------------
         -- To_Command_Break_Parent --
         -----------------------------

         function To_Command_Break_Parent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Break_Parent;

         ---------------------
         -- To_Command_Fill --
         ---------------------

         function To_Command_Fill
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Fill;

         ----------------------
         -- To_Command_Group --
         ----------------------

         function To_Command_Group
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Group;

         -------------------------
         -- To_Command_If_Break --
         -------------------------

         function To_Command_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_If_Break;

         -----------------------
         -- To_Command_Indent --
         -----------------------

         function To_Command_Indent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Indent;

         --------------------------------
         -- To_Command_Indent_If_Break --
         --------------------------------

         function To_Command_Indent_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Indent_If_Break;

         ---------------------
         -- To_Command_Line --
         ---------------------

         function To_Command_Line
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Line;

         ----------------------
         -- To_Command_Label --
         ----------------------

         function To_Command_Label
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Label;

         ----------------------------
         -- To_Command_Line_Suffix --
         ----------------------------

         function To_Command_Line_Suffix
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Line_Suffix;

         -------------------------------------
         -- To_Command_Line_Suffix_Boundary --
         -------------------------------------

         function To_Command_Line_Suffix_Boundary
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Line_Suffix_Boundary;

         ---------------------
         -- To_Command_Trim --
         ---------------------

         function To_Command_Trim
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            raise Program_Error;
            return (Kind => Command_Trim);
         end To_Command_Trim;

      begin
         case Kind (Json) is
            when JSON_Boolean_Type
                 | JSON_Int_Type
                 | JSON_Float_Type =>
               raise Program_Error;

            when JSON_Null_Type =>
               return No_Document;

            when JSON_String_Type =>
               return To_Document_Text (Json);

            when JSON_Array_Type =>
               return To_Document_List (Json);

            when JSON_Object_Type =>
               return To_Document_Command (Json);

         end case;
      end To_Document_Type;

   begin
      case Read_Result.Success is
         when True =>
            return To_Document_Type (Read_Result.Value);
         when False =>
            --  TODO: Gracefully handle this
            raise Program_Error;
      end case;
   end Deserialize;

end Gnatfmt.Documents.Json;