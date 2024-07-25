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

   ------------------------------
   -- Indentation_Continuation --
   ------------------------------

   function Indentation_Continuation
     (Self : Basic_Format_Options_Type)
      return Positive
   is
      use Optional_Positives;

   begin
      return
        Self.Indentation_Continuation
        or ((Self.Indentation
             or Default_Basic_Format_Options.Indentation.Value) - 1);
   end Indentation_Continuation;

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
      Gnatformat_Trace.Trace ("Elaborating GPR2");

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
        (Name                  => Q_Indentation_Continuation_Attribute_Id,
         Index_Type            =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                 => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive  => False,
         Is_Allowed_In         =>
           GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Indentation_Continuation_Attribute_Id,
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
      use Prettier_Ada.Documents;

      Width                     : constant Natural :=
        Self.Width or Default_Basic_Format_Options.Width.Value;
      Indentation               : constant Natural :=
        Self.Indentation or Default_Basic_Format_Options.Indentation.Value;
      Indentation_Kind          :
        constant Prettier_Ada.Documents.Indentation_Kind :=
          (case Self.Indentation_Kind.Is_Set is
             when True  =>
               (case Self.Indentation_Kind.Value is
                  when Spaces => Prettier_Ada.Documents.Spaces,
                  when Tabs   => Prettier_Ada.Documents.Tabs),
             when False =>
               (case Default_Basic_Format_Options.Indentation_Kind.Value is
                  when Spaces => Prettier_Ada.Documents.Spaces,
                  when Tabs   => Prettier_Ada.Documents.Tabs));
      Indentation_Continuation  : constant Positive :=
        Self.Indentation_Continuation
        or Default_Basic_Format_Options.Indentation_Continuation.Value;
      Indentation_Offset        :
        constant Prettier_Ada.Documents.Indentation_Offset_Type :=
          (Tabs => 0, Spaces => 0);
      End_Of_Line        : constant Prettier_Ada.Documents.End_Of_Line_Kind :=
        (case Self.End_Of_Line.Is_Set is
           when True  =>
             (case Self.End_Of_Line.Value is
                when LF   => Prettier_Ada.Documents.LF,
                when CRLF => Prettier_Ada.Documents.CRLF),
           when False =>
             (case Default_Basic_Format_Options.End_Of_Line.Value is
                when LF   => Prettier_Ada.Documents.LF,
                when CRLF => Prettier_Ada.Documents.CRLF));

   begin
      return
        Prettier_Ada.Documents.Format_Options_Type'
          (Width             => Width,
           Indentation       =>
             (Indentation_Kind,
              Indentation,
              Indentation_Continuation,
              Indentation_Offset),
           End_Of_Line       => End_Of_Line);
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

   ----------
   -- Into --
   ----------

   function Into
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type
   is (if Self.Sources.Contains (Source_Filename)
       then Self.Sources.Element (Source_Filename).Into
       else Self.Into (Language_Fallback));

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

   -----------------------------------
   -- With_Indentation_Continuation --
   -----------------------------------

   procedure With_Indentation_Continuation
     (Self                     : in out Format_Options_Builder_Type;
      Indentation_Continuation : Positive;
      Language                 : Supported_Languages)
   is
   begin
      Self.Format_Options.Language (Language).Indentation_Continuation :=
        (Is_Set => True, Value => Indentation_Continuation);
   end With_Indentation_Continuation;

   -----------------------------------
   -- With_Indentation_Continuation --
   -----------------------------------

   procedure With_Indentation_Continuation
     (Self                     : in out Format_Options_Builder_Type;
      Indentation_Continuation : Positive;
      Source_Filename          : String)
   is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self
           .Format_Options
           .Sources
           .Reference (Source_Filename)
           .Indentation_Continuation :=
           (Is_Set => True, Value => Indentation_Continuation);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Indentation_Continuation =>
               (Is_Set => True, Value => Indentation_Continuation),
             others            => <>));
      end if;
   end With_Indentation_Continuation;

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
           (End_Of_Line,
            Indentation,
            Indentation_Kind,
            Indentation_Continuation,
            Width,
            Unknown);

         type Attribute_Value_Type (Kind : Format_Option_Kind := Unknown) is
           record
             case Kind is
               when Indentation_Continuation =>
                 Indentation_Continuation : Positive;
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
            Gnatformat_Trace.Trace ("Ada attribute " & Attribute.Index.Text);

            case Attribute_Value.Kind is
               when Indentation_Continuation =>
                  Gnatformat_Trace.Trace
                    (Indentation_Continuation'Image
                     & " = "
                     & Attribute_Value.Indentation_Continuation'Image);
                  Self.With_Indentation_Continuation
                    (Attribute_Value.Indentation_Continuation, Ada_Language);

               when End_Of_Line =>
                  Gnatformat_Trace.Trace
                    (End_Of_Line'Image
                     & " = "
                     & Attribute_Value.End_Of_Line'Image);
                  Self.With_End_Of_Line
                    (Attribute_Value.End_Of_Line, Ada_Language);

               when Indentation =>
                  Gnatformat_Trace.Trace
                    (Indentation'Image
                     & " = "
                     & Attribute_Value.Indentation'Image);
                  Self.With_Indentation
                    (Attribute_Value.Indentation, Ada_Language);

               when Indentation_Kind =>
                  Gnatformat_Trace.Trace
                    (Indentation_Kind'Image
                     & " = "
                     & Attribute_Value.Indentation_Kind'Image);
                  Self.With_Indentation_Kind
                    (Attribute_Value.Indentation_Kind, Ada_Language);

               when Width =>
                  Gnatformat_Trace.Trace
                    (Width'Image
                     & " = "
                     & Attribute_Value.Width'Image);
                  Self.With_Width
                    (Attribute_Value.Width, Ada_Language);

               when Unknown =>
                  Gnatformat_Trace.Trace ("Unknown attribute");
            end case;

         else
            Gnatformat_Trace.Trace
              ("Source attribute for " & Attribute.Index.Text);

            case Attribute_Value.Kind is
               when Indentation_Continuation =>
                  Gnatformat_Trace.Trace
                    (Indentation_Continuation'Image
                     & " = "
                     & Attribute_Value.Indentation_Continuation'Image);
                  Self.With_Indentation_Continuation
                    (Attribute_Value.Indentation_Continuation,
                     Attribute.Index.Text);

               when End_Of_Line =>
                  Gnatformat_Trace.Trace
                    (End_Of_Line'Image
                     & " = "
                     & Attribute_Value.End_Of_Line'Image);
                  Self.With_End_Of_Line
                    (Attribute_Value.End_Of_Line, Attribute.Index.Text);

               when Indentation =>
                  Gnatformat_Trace.Trace
                    (Indentation'Image
                     & " = "
                     & Attribute_Value.Indentation'Image);
                  Self.With_Indentation
                    (Attribute_Value.Indentation, Attribute.Index.Text);

               when Indentation_Kind =>
                  Gnatformat_Trace.Trace
                    (Indentation_Kind'Image
                     & " = "
                     & Attribute_Value.Indentation_Kind'Image);
                  Self.With_Indentation_Kind
                    (Attribute_Value.Indentation_Kind, Attribute.Index.Text);

               when Width =>
                  Gnatformat_Trace.Trace
                    (Width'Image
                     & " = "
                     & Attribute_Value.Width'Image);
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

   ----------------------------------------------
   --  Getters for Format_Options_Type fields  --
   ----------------------------------------------

   function Into
     (Format_Options    : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages)
      return Basic_Format_Options_Type
   is (if Format_Options.Sources.Contains (Source_Filename)
       then Format_Options.Sources.Element (Source_Filename)
       else Format_Options.Language (Language_Fallback));

   -----------------
   --  Get_Width  --
   -----------------

   function Get_Width
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Natural
   is
      use Optional_Positives;
   begin
      return
        Into (Options, Source_Filename, Language).Width
        or Default_Basic_Format_Options.Width.Value;
   end Get_Width;

   -----------------------
   --  Get_Indentation  --
   -----------------------

   function Get_Indentation
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Natural
   is
      use Optional_Positives;
   begin
      return
        Into (Options, Source_Filename, Language).Indentation
        or Default_Basic_Format_Options.Indentation.Value;
   end Get_Indentation;

   ----------------------------
   --  Get_Indentation_Kind  --
   ----------------------------

   function Get_Indentation_Kind
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Indentation_Kind
   is
      Res       : Indentation_Kind;
      Basic_Opt : constant Basic_Format_Options_Type :=
        Into (Options, Source_Filename, Language);
   begin
      if Basic_Opt /= Undefined_Basic_Format_Options
        and then Basic_Opt.Indentation_Kind.Is_Set
      then
         Res := Basic_Opt.Indentation_Kind.Value;
      else
         Res := Default_Basic_Format_Options.Indentation_Kind.Value;
      end if;

      return Res;
   end Get_Indentation_Kind;

   ------------------------------------
   --  Get_Indentation_Continuation  --
   ------------------------------------

   function Get_Indentation_Continuation
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Natural
   is
      use Optional_Positives;
   begin
      return
        Into (Options, Source_Filename, Language).Indentation_Continuation
        or Default_Basic_Format_Options.Indentation_Continuation.Value;
   end Get_Indentation_Continuation;

   -----------------------
   --  Get_End_Of_Line  --
   -----------------------

   function Get_End_Of_Line
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return End_Of_Line_Kind
   is
      Res                : End_Of_Line_Kind;
      Basic_Opt : constant Basic_Format_Options_Type :=
        Into (Options, Source_Filename, Language);
   begin
      if Basic_Opt /= Undefined_Basic_Format_Options
        and then Basic_Opt.End_Of_Line.Is_Set
      then
         Res := Basic_Opt.End_Of_Line.Value;
      else
         Res := Default_Basic_Format_Options.End_Of_Line.Value;
      end if;

      return Res;
   end Get_End_Of_Line;

   -----------------------------
   --  Load_Unparsing_Config  --
   -----------------------------

   function Load_Unparsing_Configuration
     (Unparsing_Configuration_File : GNATCOLL.VFS.Virtual_File;
      Diagnostics :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration
   is
      use type GNATCOLL.VFS.Virtual_File;
      use Langkit_Support.Generic_API;

   begin
      if Unparsing_Configuration_File = GNATCOLL.VFS.No_File then
         Gnatformat.Gnatformat_Trace.Trace
           ("Using the default unparsing configuration");
         return
           Unparsing.Default_Unparsing_Configuration
             (Language => Libadalang.Generic_API.Ada_Lang_Id);
      end if;

      declare
         Rules_File_Name : constant String :=
           GNATCOLL.VFS."+" (Unparsing_Configuration_File.Full_Name);

      begin
         Gnatformat.Gnatformat_Trace.Trace
           ("Loading formatting rules from """ & Rules_File_Name & """");

         return
           Unparsing.Load_Unparsing_Config
             (Libadalang.Generic_API.Ada_Lang_Id,
              Rules_File_Name,
              Diagnostics);
      end;
   end Load_Unparsing_Configuration;

end Gnatformat.Configuration;
