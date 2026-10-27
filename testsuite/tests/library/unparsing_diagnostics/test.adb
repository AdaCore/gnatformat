--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Gnatformat.Configuration; use Gnatformat.Configuration;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Diagnostics;           use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Slocs;                 use Langkit_Support.Slocs;

with Libadalang.Generic_API.Unparsing;

procedure Test is
   use type Ada.Containers.Count_Type;

   Base        : constant Virtual_File :=
     Create_From_UTF8
       (Libadalang.Generic_API.Unparsing.Default_Configuration_Filename);
   Syntax_File : constant Virtual_File := Create_From_UTF8 ("syntax.json");
   Diagnostics : Diagnostics_Vectors.Vector;
   Config      : Unparsing_Configuration;
   Builder     : Format_Options_Builder_Type := Create_Format_Options_Builder;

   procedure Check_Syntax (Label : String);
   --  Check the public structured location as well as its rendered message.

   procedure Check_Syntax (Label : String) is
      Diagnostic : constant Langkit_Support.Diagnostics.Diagnostic :=
        Diagnostics.Last_Element;
      Message    : constant String := To_Pretty_String (Diagnostic);
   begin
      pragma Assert (Config = No_Unparsing_Configuration);
      pragma
        Assert
          (Diagnostic.Sloc_Range
           = (Start_Line   => 3,
              End_Line     => 3,
              Start_Column => 1,
              End_Column   => 1));
      pragma Assert (Message'Length > 5);
      pragma Assert (Message (Message'First .. Message'First + 4) = "3:1: ");
      Put_Line (Label & ": " & Message);
   end Check_Syntax;

begin
   Config :=
     Load_Unparsing_Configuration (Syntax_File, Empty_File_Array, Diagnostics);
   pragma Assert (Diagnostics.Length = 1);
   Check_Syntax ("Direct base");

   Diagnostics.Clear;
   Config := Load_Unparsing_Configuration (Base, [Syntax_File], Diagnostics);
   pragma Assert (Diagnostics.Length = 1);
   Check_Syntax ("Direct override");

   --  Source-specific settings use the same diagnostic channel as direct
   --  loading, including when the tall layout is applied first.
   Builder.With_Layout (Tall);
   Builder.With_Override_Layout
     ((Is_Set => True, Value => [Syntax_File]), "sample.adb");
   Diagnostics.Clear;
   Config :=
     Compute_Unparsing_Configuration
       ("sample.adb", Builder.Build, Base, Diagnostics);
   pragma Assert (Diagnostics.Length = 1);
   Check_Syntax ("Computed layout override");

   --  Preserve caller messages on a miss without storing them in the cache.
   --  Subsequent callers must receive only the cached engine diagnostics.
   declare
      Cache             : Unparsing_Configuration_Cache_Type :=
        Create_Unparsing_Configuration_Cache (Base);
      Engine_Diagnostic : Diagnostic;
   begin
      Diagnostics.Clear;
      Append (Diagnostics, Message => "Existing caller diagnostic");
      Config := Cache.Get ("sample.adb", Builder.Build, Diagnostics);
      pragma Assert (Diagnostics.Length = 2);
      pragma
        Assert
          (To_Pretty_String (Diagnostics.First_Element)
           = "Existing caller diagnostic");
      Engine_Diagnostic := Diagnostics.Last_Element;
      Check_Syntax ("Cache miss");

      Diagnostics.Clear;
      Config := Cache.Get ("sample.adb", Builder.Build, Diagnostics);
      pragma Assert (Diagnostics.Length = 1);
      pragma Assert (Diagnostics.First_Element = Engine_Diagnostic);
      Check_Syntax ("Cache hit");

      Append (Diagnostics, Message => "Another caller diagnostic");
      Config := Cache.Get ("sample.adb", Builder.Build, Diagnostics);
      pragma Assert (Diagnostics.Length = 3);
      pragma Assert (Diagnostics.Last_Element = Engine_Diagnostic);
      pragma
        Assert
          (To_Pretty_String (Diagnostics.Element (2))
           = "Another caller diagnostic");
      Put_Line ("Caller diagnostics preserved: OK");
   end;

   --  Missing and semantically invalid files have messages but no location.
   --  Keep that distinction available to IDE and other library consumers.
   Diagnostics.Clear;
   Config :=
     Load_Unparsing_Configuration
       (Create_From_UTF8 ("semantic.json"), Empty_File_Array, Diagnostics);
   pragma Assert (Config = No_Unparsing_Configuration);
   pragma Assert (Diagnostics.Length = 1);
   pragma
     Assert (Diagnostics.First_Element.Sloc_Range = No_Source_Location_Range);
   pragma
     Assert
       (To_Pretty_String (Diagnostics.First_Element)
        = "invalid node name: NoSuchNode");
   Put_Line ("Semantic diagnostic without location: OK");

   Diagnostics.Clear;
   Config :=
     Load_Unparsing_Configuration
       (Create_From_UTF8 ("missing.json"), Empty_File_Array, Diagnostics);
   pragma Assert (Config = No_Unparsing_Configuration);
   pragma Assert (Diagnostics.Length = 1);
   pragma
     Assert (Diagnostics.First_Element.Sloc_Range = No_Source_Location_Range);
   Put_Line ("Missing configuration diagnostic: OK");

   --  Reject non-object JSON roots before entering the engine: its JSON
   --  accessor exception path leaks buffers and partial configurations.
   Diagnostics.Clear;
   Append (Diagnostics, Message => "Existing caller diagnostic");
   Config :=
     Load_Unparsing_Configuration
       (Create_From_UTF8 ("shape.json"), Empty_File_Array, Diagnostics);
   pragma Assert (Config = No_Unparsing_Configuration);
   pragma Assert (Diagnostics.Length = 2);
   pragma
     Assert
       (To_Pretty_String (Diagnostics.First_Element)
        = "Existing caller diagnostic");
   pragma
     Assert (Diagnostics.Last_Element.Sloc_Range = No_Source_Location_Range);
   pragma
     Assert
       (To_Pretty_String (Diagnostics.Last_Element)
        = "unparsing configuration must be a JSON object");

   Diagnostics.Clear;
   Config :=
     Load_Unparsing_Configuration
       (Base, [Create_From_UTF8 ("shape.json")], Diagnostics);
   pragma Assert (Config = No_Unparsing_Configuration);
   pragma Assert (Diagnostics.Length = 1);
   pragma
     Assert
       (To_Pretty_String (Diagnostics.First_Element)
        = "unparsing configuration must be a JSON object");
   Put_Line ("Invalid JSON structure diagnostic: OK");

   Diagnostics.Clear;
   Config :=
     Load_Unparsing_Configuration (Base, Empty_File_Array, Diagnostics);
   pragma Assert (Config /= No_Unparsing_Configuration);
   pragma Assert (Diagnostics.Is_Empty);
   Put_Line ("Valid configuration: OK");
end Test;
