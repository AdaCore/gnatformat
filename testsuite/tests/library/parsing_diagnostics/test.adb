with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Gnatformat.Configuration; use Gnatformat.Configuration;
with Gnatformat.Edits;         use Gnatformat.Edits;
with Gnatformat.Formatting;    use Gnatformat.Formatting;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Generic_API;

procedure Test is
   use type Ada.Containers.Count_Type;

   LF      : constant Character := Ada.Characters.Latin_1.LF;
   --  Leave preprocessing inactive so directives produce parsing diagnostics.
   Context : constant Analysis_Context := Create_Context;

   procedure Check_Invalid (Unit : Analysis_Unit);
   --  Check that both Format overloads reject Unit and that Range_Format
   --  returns its diagnostics without proposing an edit.

   procedure Check_Invalid (Unit : Analysis_Unit) is
      Expected_Message : Unbounded_String;
      Text             : Unbounded_String;

      procedure Check_Range (Selection : Source_Location_Range);
      --  Preserve the original unit and diagnostic order for any selection.

      procedure Check_Range (Selection : Source_Location_Range) is
         Edit        : constant Formatting_Edit_Type :=
           Range_Format
             (Unit,
              Selection,
              Default_Format_Options,
              Default_Unparsing_Configuration);
         Diagnostics : constant Diagnostics_Array := Unit.Diagnostics;
      begin
         pragma Assert (Edit.Unit = Unit);
         pragma Assert (Edit.Text_Edit = No_Text_Edit);
         pragma Assert (Edit.Formatted_Node.Is_Null);
         pragma Assert (Edit.Diagnostics.Length = Diagnostics'Length);
         for I in Diagnostics'Range loop
            pragma
              Assert
                (To_Pretty_String (Edit.Diagnostics.Element (I))
                 = To_Pretty_String (Diagnostics (I)));
         end loop;
      end Check_Range;
   begin
      --  Build the expected exception message using the same diagnostic
      --  rendering as the CLI, including source locations and line breaks.
      pragma Assert (Unit.Has_Diagnostics);
      for Diagnostic of Unit.Diagnostics loop
         if Length (Expected_Message) > 0 then
            Append (Expected_Message, LF);
         end if;
         Append (Expected_Message, To_Pretty_String (Diagnostic));
      end loop;

      --  The Ada-specific entry point must raise instead of returning text.
      begin
         Text :=
           Format
             (Unit, Default_Format_Options, Default_Unparsing_Configuration);
         raise Program_Error with "Format accepted an invalid unit";
      exception
         when E : Formatting_Error =>
            pragma
              Assert
                (Ada.Exceptions.Exception_Message (E)
                 = To_String (Expected_Message));
      end;

      --  The generic entry point must report the same exception message.
      begin
         Text :=
           Format
             (Libadalang.Generic_API.To_Generic_Unit (Unit),
              Into (Default_Format_Options, Unit.Get_Filename),
              Default_Unparsing_Configuration);
         raise Program_Error with "Generic Format accepted an invalid unit";
      exception
         when E : Formatting_Error =>
            pragma
              Assert
                (Ada.Exceptions.Exception_Message (E)
                 = To_String (Expected_Message));
      end;

      --  Diagnostics must be checked before whole-unit dispatch or AST
      --  lookup for an explicit selection.
      Check_Range (No_Source_Location_Range);
      Check_Range ((1, 1, 1, 2));
   end Check_Invalid;
begin
   --  Inactive directives must prevent formatting even when the surrounding
   --  Ada code is otherwise valid.
   Check_Invalid
     (Context.Get_From_Buffer
        ("directives.adb",
         Buffer =>
           "procedure Demo is"
           & LF
           & "begin"
           & LF
           & "#if DEBUG then"
           & LF
           & "null;"
           & LF
           & "#else"
           & LF
           & "null;"
           & LF
           & "#end if;"
           & LF
           & "end Demo;"
           & LF));
   Put_Line ("Inactive preprocessor directives: OK");

   --  Apply the same error handling to ordinary syntax errors.
   Check_Invalid
     (Context.Get_From_Buffer ("invalid.adb", Buffer => "procedure"));
   Put_Line ("Invalid syntax: OK");

   --  A missing file also produces diagnostics and must not require an AST.
   Check_Invalid (Context.Get_From_File ("missing.adb"));
   Put_Line ("Missing source: OK");

   --  Valid units must still format normally through both Format overloads
   --  and whole-unit Range_Format, with no diagnostics.
   declare
      Unit     : constant Analysis_Unit :=
        Context.Get_From_Buffer
          ("valid.adb", Buffer => "procedure Demo is begin null; end Demo;");
      Expected : constant Unbounded_String :=
        To_Unbounded_String
          ("procedure Demo is"
           & LF
           & "begin"
           & LF
           & "   null;"
           & LF
           & "end Demo;"
           & LF);
      Edit     : constant Formatting_Edit_Type :=
        Range_Format
          (Unit,
           No_Source_Location_Range,
           Default_Format_Options,
           Default_Unparsing_Configuration);
   begin
      pragma
        Assert
          (Format
             (Unit, Default_Format_Options, Default_Unparsing_Configuration)
           = Expected);
      pragma
        Assert
          (Format
             (Libadalang.Generic_API.To_Generic_Unit (Unit),
              Into (Default_Format_Options, Unit.Get_Filename),
              Default_Unparsing_Configuration)
           = Expected);
      pragma Assert (Edit.Diagnostics.Is_Empty);
      pragma Assert (Edit.Text_Edit.Text = Expected);
      pragma Assert (not Edit.Formatted_Node.Is_Null);
   end;
   Put_Line ("Valid source: OK");
end Test;
