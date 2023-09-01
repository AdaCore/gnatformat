with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

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

   procedure Break_Parent_Group
     (Group_Stack : in out Group_Command_Type_Vector);
   --  TODO: Description

   procedure Propagate_Breaks (Document : Document_Type);
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

   -----------
   -- Print --
   -----------

   function Print (Document : Document_Type) return String
   is
      Result : Ada.Strings.Unbounded.Unbounded_String;

   begin
      Propagate_Breaks (Document);

      Gnatfmt.Documents.Builders.Reset_Document_Id;

      return Ada.Strings.Unbounded.To_String (Result);

   exception
      when others =>
         Gnatfmt.Documents.Builders.Reset_Document_Id;
         return "";
   end Print;

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

end Gnatfmt.Documents;
