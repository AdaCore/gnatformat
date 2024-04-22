--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GPR2;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;

package body Gnatformat.Configuration is

   -----------
   -- Build --
   -----------

   function Build
     (Self : Format_Options_Builder_Type)
      return Format_Options_Type
   is (Self.Format_Options);

   -----------------------------------
   -- Continuation_Line_Indentation --
   -----------------------------------

   function Continuation_Line_Indentation
     (Self : Basic_Format_Options_Type)
      return Positive
   is
      use Optional_Positives;

   begin
      return
        Self.Continuation_Line
        or ((Self.Indentation
             or Default_Basic_Format_Options.Indentation.Value) - 1);
   end Continuation_Line_Indentation;

   -----------------------------------
   -- Create_Format_Options_Builder --
   -----------------------------------

   function Create_Format_Options_Builder return Format_Options_Builder_Type is
     (Format_Options_Builder_Type'(Format_Options => <>));

   -----------------------------------------
   -- Create_Project_Format_Options_Cache --
   -----------------------------------------

   function Create_Project_Format_Options_Cache
      return Project_Format_Options_Cache_Type
   is (Project_Format_Options_Cache_Type'
         (Cache => View_Ids_To_Format_Options_Hashed_Maps.Empty_Map));

   --------------------
   -- Elaborate_GPR2 --
   --------------------

   procedure Elaborate_GPR2 is
   begin
      GPR2.Project.Registry.Pack.Add
        (Package_Id, GPR2.Project.Registry.Pack.Everywhere);

      GPR2.Project.Registry.Attribute.Add
        (Name                  => Q_Width_Attribute_Id,
         Index_Type            =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                 => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         =>
           GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Width_Attribute_Id, "Max line width");

      GPR2.Project.Registry.Attribute.Add
        (Name                  => Q_Indentation_Attribute_Id,
         Index_Type            =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                 => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         =>
           GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Indentation_Attribute_Id, "Indentation size");

      GPR2.Project.Registry.Attribute.Add
        (Name                  => Q_Indentation_Kind_Attribute_Id,
         Index_Type            =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                 => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         =>
           GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Indentation_Kind_Attribute_Id, "Indentation kind: spaces | tabs");

      GPR2.Project.Registry.Attribute.Add
        (Name                  => Q_Continuation_Line_Indentation_Attribute_Id,
         Index_Type            =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                 => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         =>
           GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Continuation_Line_Indentation_Attribute_Id,
         "Continuation Line Indentation size");

      GPR2.Project.Registry.Attribute.Add
        (Name                  => Q_End_Of_Line_Attribute_Id,
         Index_Type            =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                 => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         =>
           GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_End_Of_Line_Attribute_Id, "End of line sequence: lf | crlf");

   end Elaborate_GPR2;

   -----------------
   -- From_Project --
   -----------------

   function From_Project
     (Project : GPR2.Project.View.Object)
      return Format_Options_Type
   is
      Format_Options_Builder : Format_Options_Builder_Type :=
        Create_Format_Options_Builder;

   begin
      if Project.Has_Package (Package_Id) then
         Gnatformat_Trace.Trace ("Project has a Format package");

         for Attribute of Project.Attributes (Package_Id) loop
            Format_Options_Builder.With_From_Attribute (Attribute);
         end loop;

         return Format_Options_Builder.Build;

      else
         Gnatformat_Trace.Trace ("Project does not have a Format package");

         return Default_Format_Options;
      end if;
   end From_Project;

   ---------
   -- Get --
   ---------

   function Get
     (Self    : in out Project_Format_Options_Cache_Type;
      Project : GPR2.Project.View.Object)
      return Format_Options_Type
   is
      Id : constant GPR2.View_Ids.View_Id := Project.Id;

   begin
      if Self.Cache.Contains (Id) then
         return Self.Cache.Element (Id);

      else
         declare
            Result : constant Format_Options_Type := From_Project (Project);

         begin
            Self.Cache.Insert (Id, Result);

            return Result;
         end;
      end if;
   end Get;

   ----------
   -- Into --
   ----------

   function Into
     (Self : Basic_Format_Options_Type)
      return Prettier_Ada.Documents.Format_Options_Type
   is
      use type Optional_Positive;

      Width             : constant Natural :=
        Self.Width or Prettier_Ada.Documents.Default_Format_Options.Width;
      Indentation       : constant Natural :=
        Self.Indentation
        or Prettier_Ada.Documents.Default_Format_Options.Indentation.Width;
      Indentation_Kind  : constant Prettier_Ada.Documents.Indentation_Kind :=
        (case Self.Indentation_Kind.Is_Set is
           when True  =>
             (case Self.Indentation_Kind.Value is
                when Spaces => Prettier_Ada.Documents.Spaces,
                when Tabs   => Prettier_Ada.Documents.Tabs),
           when False =>
             Prettier_Ada.Documents.Default_Format_Options.Indentation.Kind);
      End_Of_Line       : constant Prettier_Ada.Documents.End_Of_Line_Kind :=
        (case Self.End_Of_Line.Is_Set is
           when True  =>
             (case Self.End_Of_Line.Value is
                when LF   => Prettier_Ada.Documents.LF,
                when CRLF => Prettier_Ada.Documents.CRLF),
           when False =>
             Prettier_Ada.Documents.Default_Format_Options.End_Of_Line);

   begin
      return
        Prettier_Ada.Documents.Format_Options_Type'
          (Width       => Width,
           Indentation => (Indentation_Kind, Indentation),
           End_Of_Line => End_Of_Line);
   end Into;

   ----------
   -- Into --
   ----------

   function Into
     (Self     : Format_Options_Type;
      Language : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type
   is (Self.Language (Language).Into);

   ----------
   -- Into --
   ----------

   function Into
     (Self            : Format_Options_Type;
      Source_Filename : String)
      return Prettier_Ada.Documents.Format_Options_Type
   is (if Self.Sources.Contains (Source_Filename)
       then Self.Sources.Element (Source_Filename).Into
       else Prettier_Ada.Documents.Default_Format_Options);

   --------------
   -- Override --
   --------------

   procedure Overwrite
     (Target : in out Basic_Format_Options_Type;
      Source : Basic_Format_Options_Type)
   is
      use type Optional_Positive;
      use type Optional_Indentation_Kind;
      use type Optional_End_Of_Line_Kind;

   begin
      Target.Width := Source.Width or @;
      Target.Indentation := Source.Indentation or @;
      Target.Indentation_Kind := Source.Indentation_Kind or @;
      Target.End_Of_Line := Source.End_Of_Line or @;
   end Overwrite;

   --------------
   -- Override --
   --------------

   procedure Overwrite
     (Target : in out Format_Options_Type;
      Source : Format_Options_Type)
   is
      use String_To_Basic_Format_Options_Hash_Maps;

      Source_Cursor : Cursor := Source.Sources.First;

   begin
      for Supported_Language in Supported_Languages loop
         Target
           .Language (Supported_Language)
           .Overwrite (Source.Language (Supported_Language));
      end loop;

      while Has_Element (Source_Cursor) loop
         if Target.Sources.Contains (Key (Source_Cursor)) then
            Target
              .Sources
              .Reference (Source_Cursor)
              .Overwrite (Element (Source_Cursor));

         else
            Target.Sources.Insert
              (Key (Source_Cursor), Element (Source_Cursor));
         end if;

         Next (Source_Cursor);
      end loop;
   end Overwrite;

   ----------------------------------------
   -- With_Continuation_Line_Indentation --
   ----------------------------------------

   procedure With_Continuation_Line_Indentation
     (Self                          : in out Format_Options_Builder_Type;
      Continuation_Line_Indentation : Positive;
      Language                      : Supported_Languages)
   is
   begin
      Self.Format_Options.Language (Language).Continuation_Line :=
        (Is_Set => True, Value => Continuation_Line_Indentation);
   end With_Continuation_Line_Indentation;

   ----------------------------------------
   -- With_Continuation_Line_Indentation --
   ----------------------------------------

   procedure With_Continuation_Line_Indentation
     (Self                          : in out Format_Options_Builder_Type;
      Continuation_Line_Indentation : Positive;
      Source_Filename               : String)
   is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self
           .Format_Options
           .Sources
           .Reference (Source_Filename)
           .Continuation_Line :=
           (Is_Set => True, Value => Continuation_Line_Indentation);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Continuation_Line =>
               (Is_Set => True, Value => Continuation_Line_Indentation),
             others            => <>));
      end if;
   end With_Continuation_Line_Indentation;

   ----------------------
   -- With_End_Of_Line --
   ----------------------

   procedure With_End_Of_Line
     (Self            : in out Format_Options_Builder_Type;
      End_Of_Line     : End_Of_Line_Kind;
      Source_Filename : String)
   is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).End_Of_Line :=
           (Is_Set => True, Value => End_Of_Line);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (End_Of_Line => (Is_Set => True, Value => End_Of_Line),
             others      => <>));
      end if;
   end With_End_Of_Line;

   ----------------------
   -- With_End_Of_Line --
   ----------------------

   procedure With_End_Of_Line
     (Self        : in out Format_Options_Builder_Type;
      End_Of_Line : End_Of_Line_Kind;
      Language    : Supported_Languages)
   is
   begin
      Self.Format_Options.Language (Language).End_Of_Line :=
        (Is_Set => True, Value => End_Of_Line);
   end With_End_Of_Line;

   -------------------------
   -- With_From_Attribute --
   -------------------------

   procedure With_From_Attribute
     (Self      : in out Format_Options_Builder_Type;
      Attribute : GPR2.Project.Attribute.Object)
   is
      use type GPR2.Project.Attribute_Index.Object;
      use type GPR2.Project.Registry.Attribute.Value_Kind;

      Ada_Attribute_Index : constant GPR2.Project.Attribute_Index.Object :=
        GPR2.Project.Attribute_Index.Create (GPR2.Ada_Language);

      procedure Parse_Attribute
        (Attribute  : GPR2.Project.Attribute.Object);
      --  Parses Attribute value by checking if it's one of the valid
      --  kinds and then converting to the expected value type. If parsed
      --  successfully, Self.Format_Options is updated accordingly.

      ---------------------
      -- Parse_Attribute --
      ---------------------

      procedure Parse_Attribute
        (Attribute  : GPR2.Project.Attribute.Object)
      is
         use type GPR2.Q_Optional_Attribute_Id;

         type Format_Option_Kind is
           (Continuation_Line,
            End_Of_Line,
            Indentation,
            Indentation_Kind,
            Width,
            Unknown);

         type Attribute_Value_Type (Kind : Format_Option_Kind := Unknown) is
           record
             case Kind is
               when Continuation_Line =>
                 Continuation_Line : Positive;
               when End_Of_Line =>
                 End_Of_Line : End_Of_Line_Kind;
               when Indentation =>
                 Indentation : Positive;
               when Indentation_Kind =>
                 Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
               when Width =>
                 Width : Positive;
               when Unknown =>
                 null;
             end case;
           end record;

         function Parse_Attribute_Value return Attribute_Value_Type;
         --  Parses Attribute value by checking if it's one of the valid
         --  kinds and then converting to the expected value type.

         -------------------------
         -- Get_Attribute_Value --
         -------------------------

         function Parse_Attribute_Value return Attribute_Value_Type
         is
            Q_Attribute_Id : constant GPR2.Q_Optional_Attribute_Id :=
              Attribute.Name.Id;

            Raw_Attribute_Value : constant String := Attribute.Value.Text;

         begin
            if Q_Attribute_Id = Q_Width_Attribute_Id then
               return
                 (Kind  => Width,
                  Width => Positive'Value (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Indentation_Attribute_Id then
               return
                 (Kind        => Indentation,
                  Indentation => Positive'Value (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Indentation_Kind_Attribute_Id then
               return
                 (Kind             => Indentation_Kind,
                  Indentation_Kind =>
                    Gnatformat.Configuration.Indentation_Kind'Value
                       (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_End_Of_Line_Attribute_Id then
               return
                 (Kind        => End_Of_Line,
                  End_Of_Line => End_Of_Line_Kind'Value (Raw_Attribute_Value));

            else
               return (Kind => Unknown);
            end if;

         exception
            when others =>
               Gnatformat_Trace.Trace ("Failed to parse attribute value");
               return (Kind => Unknown);
         end Parse_Attribute_Value;

         Attribute_Value : constant Attribute_Value_Type :=
           Parse_Attribute_Value;

      begin
         if Attribute.Index = Ada_Attribute_Index then
            Gnatformat_Trace.Trace ("Ada attribute");

            case Attribute_Value.Kind is
               when Continuation_Line =>
                  Self.With_Continuation_Line_Indentation
                    (Attribute_Value.Continuation_Line, Ada_Language);

               when End_Of_Line =>
                  Self.With_End_Of_Line
                    (Attribute_Value.End_Of_Line, Ada_Language);

               when Indentation =>
                  Self.With_Indentation
                    (Attribute_Value.Indentation, Ada_Language);

               when Indentation_Kind =>
                  Self.With_Indentation_Kind
                    (Attribute_Value.Indentation_Kind, Ada_Language);

               when Width =>
                  Self.With_Width
                    (Attribute_Value.Width, Ada_Language);

               when Unknown =>
                  Gnatformat_Trace.Trace ("Unknown attribute");
            end case;

         else
            Gnatformat_Trace.Trace ("Source attribute");

            case Attribute_Value.Kind is
               when Continuation_Line =>
                  Self.With_Continuation_Line_Indentation
                    (Attribute_Value.Continuation_Line, Attribute.Index.Text);

               when End_Of_Line =>
                  Self.With_End_Of_Line
                    (Attribute_Value.End_Of_Line, Attribute.Index.Text);

               when Indentation =>
                  Self.With_Indentation
                    (Attribute_Value.Indentation, Attribute.Index.Text);

               when Indentation_Kind =>
                  Self.With_Indentation_Kind
                    (Attribute_Value.Indentation_Kind, Attribute.Index.Text);

               when Width =>
                  Self.With_Width
                    (Attribute_Value.Width, Attribute.Index.Text);

               when Unknown =>
                  Gnatformat_Trace.Trace ("Unknown attribute");
            end case;
         end if;
      end Parse_Attribute;

   begin
      Gnatformat_Trace.Trace
        ("Parsing attribute """ & Attribute.Image & """");

      if Attribute.Has_Index then
         case Attribute.Kind is
            when GPR2.Project.Registry.Attribute.Single =>
               Parse_Attribute (Attribute);

            when GPR2.Project.Registry.Attribute.List =>
               Gnatformat_Trace.Trace
                 ("Attribute does not have a single value");
         end case;

      else
         Gnatformat_Trace.Trace ("Invalid attribute");
      end if;
   end With_From_Attribute;

   ----------------------
   -- With_Indentation --
   ----------------------

   procedure With_Indentation
     (Self            : in out Format_Options_Builder_Type;
      Indentation     : Positive;
      Source_Filename : String)
   is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).Indentation :=
           (Is_Set => True, Value => Indentation);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Indentation => (Is_Set => True, Value => Indentation),
             others      => <>));
      end if;
   end With_Indentation;

   ----------------------
   -- With_Indentation --
   ----------------------

   procedure With_Indentation
     (Self        : in out Format_Options_Builder_Type;
      Indentation : Positive;
      Language    : Supported_Languages)
   is
   begin
      Self.Format_Options.Language (Language).Indentation :=
        (Is_Set => True, Value => Indentation);
   end With_Indentation;

   ---------------------------
   -- With_Indentation_Kind --
   ---------------------------

   procedure With_Indentation_Kind
     (Self             : in out Format_Options_Builder_Type;
      Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
      Source_Filename  : String)
   is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self
           .Format_Options
           .Sources.Reference (Source_Filename)
           .Indentation_Kind := (Is_Set => True, Value => Indentation_Kind);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Indentation_Kind => (Is_Set => True, Value => Indentation_Kind),
             others           => <>));
      end if;
   end With_Indentation_Kind;

   ---------------------------
   -- With_Indentation_Kind --
   ---------------------------

   procedure With_Indentation_Kind
     (Self             : in out Format_Options_Builder_Type;
      Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
      Language         : Supported_Languages)
   is
   begin
      Self.Format_Options.Language (Language).Indentation_Kind :=
        (Is_Set => True, Value => Indentation_Kind);
   end With_Indentation_Kind;

   ----------------
   -- With_Width --
   ----------------

   procedure With_Width
     (Self            : in out Format_Options_Builder_Type;
      Width           : Positive;
      Source_Filename : String)
   is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).Width :=
           (Is_Set => True, Value => Width);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Width  => (Is_Set => True, Value => Width),
             others => <>));
      end if;
   end With_Width;

   ----------------
   -- With_Width --
   ----------------

   procedure With_Width
     (Self     : in out Format_Options_Builder_Type;
      Width    : Positive;
      Language : Supported_Languages)
   is
   begin
      Self.Format_Options.Language (Language).Width :=
        (Is_Set => True, Value => Width);
   end With_Width;

end Gnatformat.Configuration;
