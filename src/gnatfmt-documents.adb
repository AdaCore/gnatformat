with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

with Gnatfmt.Documents.Builders;
with Gnatfmt.Optionals;
with Gnatfmt.Hashes;

package body Gnatfmt.Documents is

   Current_Symbol : Symbol_Type := Symbol_Type (Integer'First + 1);

   ----------------
   -- New_Symbol --
   ----------------

   function New_Symbol return Symbol_Type is
   begin
      return S : constant Symbol_Type := Current_Symbol do
         Current_Symbol := Current_Symbol + 1;
      end return;
   end New_Symbol;

   package Optional_Booleans is new
     Gnatfmt.Optionals.Generic_Optional_Types (Boolean);

   subtype Optional_Boolean is Optional_Booleans.Generic_Optional_Type;

   package Document_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Document_Type);

   subtype Document_Type_Vector is
     Document_Type_Vectors.Vector;

   package Group_Command_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Command_Type_Access);

   subtype Group_Command_Type_Vector is Group_Command_Type_Vectors.Vector;

   package Document_Type_Hashed_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Document_Type,
        Hash                => Hash,
        Equivalent_Elements => "=");

   subtype Document_Type_Hashed_Set is
     Document_Type_Hashed_Sets.Set;

   type Mode_Kind is (Mode_Break, Mode_Flat);

   type Indentation_Type is record
      Value  : Ada.Strings.Unbounded.Unbounded_String;
      Length : Natural;
      Queue  : Document_Type_Vector;
   end record;

   type Print_Command_Type is record
      Indentation : Indentation_Type;
      Mode        : Mode_Kind;
      Document    : Document_Type;
   end record;

   package Print_Command_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Print_Command_Type);

   subtype Print_Command_Type_Vector is Print_Command_Type_Vectors.Vector;

   package Symbol_To_Mode_Maps is new
     Ada.Containers.Hashed_Maps (Symbol_Type, Mode_Kind, Hash, "=");

   subtype Symbol_To_Mode_Map is Symbol_To_Mode_Maps.Map;

   procedure Break_Parent_Group
     (Group_Stack : in out Group_Command_Type_Vector);
   --  TODO: Description

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Natural;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map)
      return Boolean;
   --  TODO: Description

   procedure Propagate_Breaks (Document : Document_Type);
   --  TODO: Description

   function Make_Align
     (From       : Indentation_Type;
      Align_Kind : Align_Kind_Type;
      Options    : Format_Options_Type)
      return Indentation_Type;
   --  TODO: Description

   function Make_Indentation
     (From : Indentation_Type;
      Options : Indentation_Options_Type)
      return Indentation_Type;
   --  TODO: Description

   procedure Traverse_Document
     (Document                           : Document_Type;
      On_Enter                           :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      On_Exit                            :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      Should_Traverse_Conditional_Groups : Boolean := False);
   --  TODO: Description
   --  TODO: Refactor so that Optional_Boolean is not needed

   function Trim
     (Text : Ada.Strings.Unbounded.Unbounded_String)
      return Natural;
   --  TODO: Description

   ------------------------
   -- Break_Parent_Group --
   ------------------------

   procedure Break_Parent_Group
     (Group_Stack : in out Group_Command_Type_Vector) is
      use type Ada.Containers.Count_Type;

   begin
      if Group_Stack.Length > 0 then
         declare
            Parent_Group                                       :
              constant Group_Command_Type_Vectors.Reference_Type :=
                Group_Stack.Reference (Group_Stack.Last);

         begin
            if (Parent_Group.Expanded_States /= No_Document
                and then Parent_Group.Expanded_States.Bare_Document.List
                         /= null
                and then Parent_Group.Expanded_States.Bare_Document.List'Length
                       /= 0)
              and not Parent_Group.Break
            then
               --  TODO: Why should this be "propagated"?
               Parent_Group.Break := True;
            end if;
         end;
      end if;
   end Break_Parent_Group;

   ----------
   -- Fits --
   ----------

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Natural;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map)
      return Boolean
   is
      pragma Unreferenced (Next, Rest_Commands, Width, Has_Line_Suffix, Group_Mode_Map);

   begin
      raise Program_Error;
      return False;
   end Fits;

   ----------
   -- Hash --
   ----------

   function Hash
     (Document : Document_Type)
      return Ada.Containers.Hash_Type
   is
      function Bare_Document_Access_Type_Hash is new
        Gnatfmt.Hashes.Hash_Access
          (Bare_Document_Type, Bare_Document_Type_Access);
   begin
      return Bare_Document_Access_Type_Hash (Document.Bare_Document);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Symbol));

   ------------
   -- Format --
   ------------

   function Format
     (Document : Document_Type;
      Options : Format_Options_Type)
      return String
   is
      use type Ada.Containers.Count_Type;

      Group_Mode_Map : Symbol_To_Mode_Map;

      Pos : Natural := 0;

      Should_Remeasure : Boolean := False;

      Line_Suffix : constant Print_Command_Type_Vector := [];

      Print_Commands : Print_Command_Type_Vector :=
         [Print_Command_Type'
            (Indentation_Type'
               (Ada.Strings.Unbounded.Null_Unbounded_String, 0, []),
             Mode_Break,
             Document)];

      Result : Ada.Strings.Unbounded.Unbounded_String;

   begin
      Propagate_Breaks (Document);

      while Print_Commands.Length > 0 loop
         declare
            Print_Command : constant Print_Command_Type :=
              Print_Commands.Last_Element;
            Indentation   : Indentation_Type renames Print_Command.Indentation;
            Mode          : Mode_Kind renames Print_Command.Mode;
            Document      : Document_Type renames Print_Command.Document;

         begin
            Print_Commands.Delete_Last;

            case Document.Bare_Document.Kind is
               when Document_Text =>
                  declare
                     --  TODO: Do '\n' convertions to '\r\n\' if necessary
                     Formatted :
                       constant Ada.Strings.Unbounded.Unbounded_String :=
                         Document.Bare_Document.Text;

                  begin
                     Ada.Strings.Unbounded.Append (Result, Formatted);
                     if Print_Commands.Length > 0 then
                        Pos := @ + Ada.Strings.Unbounded.Length (Formatted);
                     end if;
                  end;

               when Document_List =>
                  for Command of
                     reverse Document.Bare_Document.List.all
                  loop
                     Print_Commands.Append
                       (Print_Command_Type'(Indentation, Mode, Command));
                  end loop;

               when Document_Command =>
                  case Document.Bare_Document.Command.Kind is
                     when Command_Cursor =>
                        raise Program_Error;

                     when Command_Indent =>
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Indentation
                                (Indentation, Options.Indentation),
                              Mode,
                              Document.Bare_Document.Command.Indent_Contents));

                     when Command_Align =>
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Align
                                (Indentation,
                                 Document.Bare_Document.Command.Align_Kind,
                                 Options),
                              Mode,
                              Document.Bare_Document.Command.Align_Contents));

                     when Command_Trim =>
                        Pos := @ - Trim (Result);

                     when Command_Group =>
                        declare
                           procedure Process_Mode_Break;
                           --  TODO

                           procedure Process_Mode_Flat;
                           --  TODO

                           ------------------------
                           -- Process_Mode_Break --
                           ------------------------

                           procedure Process_Mode_Break is
                              Next : constant Print_Command_Type :=
                                (Indentation => Indentation,
                                 Mode        => Mode_Flat,
                                 Document    =>
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Group_Contents);
                              Remainder : constant Natural := Options.Width - Pos;
                              Has_Line_Suffix : constant Boolean := Line_Suffix.Length > 0;

                           begin
                              Should_Remeasure := False;

                              if not Document.Bare_Document.Command.Break
                                 and then
                                   Fits
                                     (Next,
                                      Print_Commands,
                                      Remainder,
                                      Has_Line_Suffix,
                                      Group_Mode_Map)
                              then
                                 Print_Commands.Append (Next);

                              else
                                 if Document.Bare_Document.Command.Expanded_States.Bare_Document /= null then
                                    declare
                                       Group_Contents : Document_Type renames
                                         Document.Bare_Document.Command.Group_Contents;
                                       Expanded_States : Document_Type_Array_Access renames
                                         Document.Bare_Document.Command.Expanded_States.Bare_Document.List;
                                       Most_Expanded : constant Document_Type := Expanded_States (Expanded_States'Last);

                                    begin
                                       if Document.Bare_Document.Command.Break then
                                          Print_Commands.Append
                                            (Print_Command_Type'
                                               (Indentation,
                                                Mode_Break,
                                                Most_Expanded));

                                       else
                                          for J in Expanded_States'First .. Expanded_States'Last - 1 loop
                                             declare
                                                State         : constant Document_Type := Expanded_States (J);
                                                Print_Command : constant Print_Command_Type :=
                                                  (Indentation, Mode_Flat, State);
                                             begin
                                                if Fits
                                                     (Print_Command,
                                                      Print_Commands,
                                                      Remainder,
                                                      Has_Line_Suffix,
                                                      Group_Mode_Map)
                                                then
                                                   Print_Commands.Append
                                                     (Print_Command_Type'
                                                        (Indentation, Mode_Break, Group_Contents));
                                                end if;
                                             end;
                                          end loop;

                                          Print_Commands.Append
                                            (Print_Command_Type'
                                               (Indentation,
                                                Mode_Break,
                                                Most_Expanded));
                                       end if;
                                    end;

                                 else
                                    Print_Commands.Append
                                      (Print_Command_Type'
                                         (Indentation,
                                          Mode_Break,
                                          Document.Bare_Document.Command.Group_Contents));
                                 end if;
                              end if;
                           end Process_Mode_Break;

                           -----------------------
                           -- Process_Mode_Flat --
                           -----------------------

                           procedure Process_Mode_Flat is
                           begin
                              if not Should_Remeasure then
                                 Print_Commands.Append
                                    (Print_Command_Type'
                                       (Indentation,
                                       (if Document
                                             .Bare_Document
                                             .Command
                                             .Break
                                          then
                                          Mode_Break
                                          else
                                          Mode_Flat),
                                       Document
                                          .Bare_Document
                                          .Command
                                          .Group_Contents));
                              else
                                 Process_Mode_Break;
                              end if;
                           end Process_Mode_Flat;

                        begin
                           case Mode is
                              when Mode_Flat =>
                                 Process_Mode_Flat;

                              when Mode_Break =>
                                 Process_Mode_Break;

                           end case;
                        end;

                     when Command_Fill =>
                        raise Program_Error;

                     when others =>
                        raise Program_Error;
                  end case;

            end case;
         end;
      end loop;

      Gnatfmt.Documents.Builders.Reset_Document_Id;

      return Ada.Strings.Unbounded.To_String (Result);

   exception
      when others =>
         Gnatfmt.Documents.Builders.Reset_Document_Id;
         return "";
   end Format;

   ----------------------
   -- Make_Indentation --
   ----------------------

   function Make_Indentation
     (From : Indentation_Type;
      Options : Indentation_Options_Type)
      return Indentation_Type
   is
      pragma Unreferenced (Options);
      R : constant Indentation_Type := From;
   begin
      raise Program_Error;
      return R;
   end Make_Indentation;

   ----------------
   -- Make_Align --
   ----------------

   function Make_Align
     (From       : Indentation_Type;
      Align_Kind : Align_Kind_Type;
      Options    : Format_Options_Type)
      return Indentation_Type
   is
      pragma Unreferenced (Options, Align_Kind);
      R : constant Indentation_Type := From;
   begin
      raise Program_Error;
      return R;
   end Make_Align;

   ----------------------
   -- Propagate_Breaks --
   ----------------------

   procedure Propagate_Breaks (Document : Document_Type) is
      Already_Visited : Document_Type_Hashed_Set;
      Group_Stack     : Group_Command_Type_Vector;

      function Propagate_Breaks_On_Enter
        (Document : Document_Type)
         return Optional_Boolean;
      --  TODO

      function Propagate_Breaks_On_Exit
        (Document : Document_Type)
         return Optional_Boolean;
      --  TODO

      -------------------------------
      -- Propagate_Breaks_On_Enter --
      -------------------------------

      function Propagate_Breaks_On_Enter
        (Document : Document_Type)
         return Optional_Boolean
      is

      begin
         case Document.Bare_Document.Kind is
            when Document_Command =>
               case Document.Bare_Document.Command.Kind is
                  when Command_Break_Parent =>
                     Break_Parent_Group (Group_Stack);

                  when Command_Group =>
                     Group_Stack.Append (Document.Bare_Document.Command);
                     if Already_Visited.Contains (Document) then
                        return
                          Optional_Boolean'
                            (Is_Set => True, Value => False);
                     else
                        Already_Visited.Insert (Document);
                     end if;

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;

         return Optional_Boolean'(Is_Set => False);
      end Propagate_Breaks_On_Enter;

      ------------------------------
      -- Propagate_Breaks_On_Exit --
      ------------------------------

      function Propagate_Breaks_On_Exit
        (Document : Document_Type)
         return Optional_Boolean
      is
      begin
         case Document.Bare_Document.Kind is
            when Document_Command =>
               if Document.Bare_Document.Command.Kind in Command_Group then
                  declare
                     Group : constant Command_Type_Access :=
                       Group_Stack.Last_Element;

                  begin
                     Group_Stack.Delete_Last;
                     if Group.Break then
                        Break_Parent_Group (Group_Stack);
                     end if;
                  end;
               end if;

            when others =>
               null;
         end case;

         return Optional_Boolean'(Is_Set => False);
      end Propagate_Breaks_On_Exit;

   begin
      Traverse_Document
        (Document                           => Document,
         On_Enter                           => Propagate_Breaks_On_Exit'Access,
         On_Exit                            =>
           Propagate_Breaks_On_Enter'Access,
         Should_Traverse_Conditional_Groups => True);
   end Propagate_Breaks;

   -----------------------
   -- Traverse_Document --
   -----------------------

   procedure Traverse_Document
     (Document                           : Document_Type;
      On_Enter                           :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      On_Exit                            :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      Should_Traverse_Conditional_Groups : Boolean := False)
   is
      use type Ada.Containers.Count_Type;

      Traverse_Doc_On_Exit_Stack_Marker : constant Document_Type :=
        Document_Type'(Bare_Document => null);

      Doc_Stack : Document_Type_Vector := [Document];

   begin
      while Doc_Stack.Length > 0 loop
         <<Continue>>
         declare
            Doc : Document_Type :=
              Doc_Stack.Last_Element;

         begin
            Doc_Stack.Delete_Last;

            if Doc = Traverse_Doc_On_Exit_Stack_Marker then
               Doc := Doc_Stack.Last_Element;
               Doc_Stack.Delete_Last;
               declare
                  Ignore : constant Optional_Boolean :=
                    On_Exit (Doc);
               begin
                  goto Continue;
               end;
            end if;

            if On_Exit /= null then
               Doc_Stack.Append (Traverse_Doc_On_Exit_Stack_Marker);
            end if;

            if On_Enter /= null then
               declare
                  On_Enter_Result : constant Optional_Boolean :=
                    On_Enter (Doc);
               begin
                  if On_Enter_Result.Is_Set
                    and then not On_Enter_Result.Value
                  then
                     goto Continue;
                  end if;
               end;
            end if;

            case Doc.Bare_Document.Kind is
               when Document_Text =>
                  null;

               when Document_List =>
                  for Child_Doc of reverse Doc.Bare_Document.List.all loop
                     Doc_Stack.Append (Child_Doc);
                  end loop;

               when Document_Command =>
                  case Doc.Bare_Document.Command.Kind is
                     when Command_Align =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Align_Contents);

                     when Command_Indent =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Indent_Contents);

                     when Command_Indent_If_Break =>
                        Doc_Stack.Append
                          (Doc
                             .Bare_Document
                             .Command
                             .Indent_If_Break_Contents);

                     when Command_Label =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Label_Contents);

                     when Command_Line_Suffix =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Line_Suffix_Contents);

                     when Command_Fill =>
                        for Child_Doc of reverse
                          --  TODO: How to make this safe? Is Parts always a
                          --  list?
                          Doc
                            .Bare_Document
                            .Command
                            .Parts
                            .Bare_Document
                            .List
                            .all
                        loop
                           Doc_Stack.Append (Child_Doc);
                        end loop;

                     when Command_Group =>
                        if Should_Traverse_Conditional_Groups then
                           for Child_Doc of reverse
                             --  TODO: How to make this safe? Is
                             --  Expanded_States always a list?
                             Doc
                               .Bare_Document
                               .Command
                               .Expanded_States
                               .Bare_Document
                               .List
                               .all
                           loop
                              Doc_Stack.Append (Child_Doc);
                           end loop;

                        else
                           Doc_Stack.Append
                             (Doc.Bare_Document.Command.Group_Contents);
                        end if;

                     when Command_If_Break =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Flat_Contents);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Break_Contents);

                     when Command_Break_Parent
                          | Command_Cursor
                          | Command_Line
                          | Command_Soft_Line
                          | Command_Hard_Line
                          | Command_Literal_Line
                          | Command_Line_Suffix_Boundary
                          | Command_Trim =>
                        null;

                  end case;

            end case;
         end;
      end loop;
   end Traverse_Document;

   ----------
   -- Trim --
   ----------

   function Trim
     (Text : Ada.Strings.Unbounded.Unbounded_String)
      return Natural
   is
      pragma Unreferenced (Text);
   begin
      raise Program_Error;
      return 0;
   end Trim;

end Gnatfmt.Documents;
