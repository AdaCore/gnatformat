--
--  Copyright (C) 2024-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO;

with GNAT.Traceback.Symbolic;

with GNATCOLL;

with GPR2;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;

with Libadalang.Generic_API;
with Libadalang.Generic_API.Unparsing;

package body Gnatformat.Configuration is

   function Compute_Unparsing_Configuration
     (Format_Options                  : Basic_Format_Options_Type;
      Default_Unparsing_Configuration : GNATCOLL.VFS.Virtual_File;
      Diagnostics                     :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
   --  Computes an unparsing configuration using
   --  Default_Unparsing_Configuration as based and overriding from
   --  Format_Options.

   function Get_Keyword_Casing_Overriding
     (Keyword_Casing : Keyword_Casing_Kind) return GNATCOLL.VFS.Virtual_File;
   --  Converts Keyword_Casing to the associated overriding unparsing
   --  configuration.

   function "="
     (Left, Right :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration)
      return Boolean
   is (False);
   --  Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration does not
   --  provide "=". This function is only used for comparing
   --  Unparsing_Configuration objects in a hash map, therefore, consider them
   --  as always different.

   ----------------------
   --  Get_Layout_File --
   ----------------------

   function Get_Layout_File
     (Actual : Gnatformat.Configuration.Layout_Kind)
      return GNATCOLL.VFS.Virtual_File
   is (if Actual = Tall
       then Libadalang.Generic_API.Unparsing.Builtin_Overridings.Tall
       else GNATCOLL.VFS.No_File);
   --  Returns the predefined tall layout file if the actual layout is set
   --  to Tall through switch or GPR attribute or No_File otherwise.

   -------------------------------------
   -- Compute_Unparsing_Configuration --
   -------------------------------------

   function Compute_Unparsing_Configuration
     (Format_Options                  : Basic_Format_Options_Type;
      Default_Unparsing_Configuration : GNATCOLL.VFS.Virtual_File;
      Diagnostics                     :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration
   is
      -------------------------------------
      --  Update_Overridings_File_Array  --
      -------------------------------------

      procedure Update_Overridings_File_Array
        (FA   : in out GNATCOLL.VFS.File_Array;
         Last : in out Natural;
         Val  : GNATCOLL.VFS.Virtual_File);
      --  Inserts a Virtual file in the file array of overridings

      procedure Update_Overridings_File_Array
        (FA   : in out GNATCOLL.VFS.File_Array;
         Last : in out Natural;
         Val  : GNATCOLL.VFS.Virtual_File) is
      begin
         Last := Last + 1;
         FA (Last) := Val;
      end Update_Overridings_File_Array;

      Overridings : GNATCOLL.VFS.File_Array := GNATCOLL.VFS.Empty_File_Array;
      Last : Natural := 0;
   begin
      --  Handles layout if set
      if Format_Options.Layout.Is_Set
        and then
          Get_Layout_File (Format_Options.Layout.Value) /= GNATCOLL.VFS.No_File
      then
         Update_Overridings_File_Array
           (Overridings,
            Last,
            Get_Layout_File (Format_Options.Layout.Value));
      end if;

      --  Handles keyword casing if set
      if Format_Options.Keyword_Casing.Is_Set then
         Update_Overridings_File_Array
           (Overridings,
            Last,
            Get_Keyword_Casing_Overriding
              (Format_Options.Keyword_Casing.Value));
      end if;

       --  Handles the files specified through override layout if set
      if Format_Options.Override_Layout.Is_Set then
         for F of Format_Options.Override_Layout.Value loop
            Update_Overridings_File_Array (Overridings, Last, F);
         end loop;
      end if;

      return
        Load_Unparsing_Configuration
          (Unparsing_Configuration_File => Default_Unparsing_Configuration,
           Overriddings                 => Overridings,
           Diagnostics                  => Diagnostics);
   end Compute_Unparsing_Configuration;

   -------------------------------------
   -- Compute_Unparsing_Configuration --
   -------------------------------------

   function Compute_Unparsing_Configuration
     (Source                          : String;
      Format_Options                  : Format_Options_Type;
      Default_Unparsing_Configuration : GNATCOLL.VFS.Virtual_File;
      Diagnostics                     :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration
   is
      Basic_Format_Options : constant Basic_Format_Options_Type :=
        (if Format_Options.Sources.Contains (Source)
         then Format_Options.Sources.Element (Source)
         else Format_Options.Language (Ada_Language));

   begin
      return
        Compute_Unparsing_Configuration
          (Format_Options                  => Basic_Format_Options,
           Default_Unparsing_Configuration => Default_Unparsing_Configuration,
           Diagnostics                     => Diagnostics);
   end Compute_Unparsing_Configuration;

   -----------------------------------
   -- Get_Keyword_Casing_Overriding --
   -----------------------------------

   function Get_Keyword_Casing_Overriding
     (Keyword_Casing : Keyword_Casing_Kind) return GNATCOLL.VFS.Virtual_File
   is ((case Keyword_Casing is
          when Keep  =>
            Libadalang
              .Generic_API
              .Unparsing
              .Builtin_Overridings
              .Original_Keywords,
          when Lower =>
            Libadalang
              .Generic_API
              .Unparsing
              .Builtin_Overridings
              .Lowercase_Keywords,
          when Upper =>
            Libadalang
              .Generic_API
              .Unparsing
              .Builtin_Overridings
              .Uppercase_Keywords));

   -----------
   -- Build --
   -----------

   function Build
     (Self : Format_Options_Builder_Type) return Format_Options_Type
   is (Self.Format_Options);

   -----------------------------------
   -- Create_Format_Options_Builder --
   -----------------------------------

   function Create_Format_Options_Builder
     (Project                           : Optional_GPR2_Project_View :=
        (Is_Set => False);
      Implicit_Indentation_Continuation : Boolean := True)
      return Format_Options_Builder_Type
   is (Format_Options_Builder_Type'
         (Project                           => Project,
          Format_Options                    => Undefined_Format_Options,
          Implicit_Indentation_Continuation =>
            Implicit_Indentation_Continuation));

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
        (Name                 => Q_Width_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Width_Attribute_Id, "Max line width");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Indentation_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Indentation_Attribute_Id, "Indentation size");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Indentation_Kind_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Indentation_Kind_Attribute_Id, "Indentation kind: spaces | tabs");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Indentation_Continuation_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Indentation_Continuation_Attribute_Id,
         "Continuation Line Indentation size");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_End_Of_Line_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_End_Of_Line_Attribute_Id, "End of line sequence: lf | crlf");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Charset_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Charset_Attribute_Id, "Charset to use for source decoding");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Ignore_Attribute_Id,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Charset_Attribute_Id, "Ignore file with the sources to ignore");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Keyword_Casing_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Keyword_Casing_Attribute_Id,
         "Keyword casing: keep | lower | upper");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Layout_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Layout_Attribute_Id,
         "Layout configuration: default | tall (default value = default)");

      GPR2.Project.Registry.Attribute.Add
        (Name                 => Q_Override_Layout_Attribute_Id,
         Index_Type           =>
           GPR2.Project.Registry.Attribute.FileGlob_Or_Language_Index,
         Value                => GPR2.Project.Registry.Attribute.List,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Q_Override_Layout_Attribute_Id,
         "Override the configuration for the nodes defined in the JSON files");

   end Elaborate_GPR2;

   -----------------
   -- From_Project --
   -----------------

   function From_Project
     (Project : GPR2.Project.View.Object) return Format_Options_Type
   is
      package Indexes_Hash is new
        Ada.Containers.Indefinite_Hashed_Sets (String, Ada.Strings.Hash, "=");
      subtype Indexes_Set is Indexes_Hash.Set;

      function Create_Implicit_Indentation_Continuation_Indexes
        (Indentation_Attributes : GPR2.Project.Attribute.Set.Object)
         return Indexes_Set;
      --  Creates the set of Indentation attribute indexes which needs an
      --  implicit Indentation-Continuation attribute to be computated and set

      ------------------------------------------------------
      -- Create_Implicit_Indentation_Continuation_Indexes --
      ------------------------------------------------------

      function Create_Implicit_Indentation_Continuation_Indexes
        (Indentation_Attributes : GPR2.Project.Attribute.Set.Object)
         return Indexes_Set
      is
         Implicit_Indexes : Indexes_Set := Indexes_Hash.Empty_Set;
      begin
         for Attribute of Indentation_Attributes loop
            if not Project.Attributes (Package_Id).Contains
                     (Indentation_Continuation_Attribute_Id,
                      GPR2.Project.Attribute_Index.Create
                        (Attribute.Index.Value))
            then
               Indexes_Hash.Insert
                 (Implicit_Indexes, String (Attribute.Index.Value));
            end if;
         end loop;
         return Implicit_Indexes;
      end Create_Implicit_Indentation_Continuation_Indexes;

      Indexes : Indexes_Set := Indexes_Hash.Empty_Set;

   begin
      if Project.Has_Package (Package_Id) then
         Gnatformat_Trace.Trace ("Project has a Format package");

         --  Compute the implicit Indentation Continuation attribute indexes
         Indexes :=
           Create_Implicit_Indentation_Continuation_Indexes
             (GPR2.Project.View.Attributes
                (Project, Q_Indentation_Attribute_Id));

         declare
            Format_Options_Builder : Format_Options_Builder_Type :=
              Create_Format_Options_Builder
                ((Is_Set => True, Value => Project),
                 Implicit_Indentation_Continuation => not Indexes.Is_Empty);
         begin

            for Attribute of Project.Attributes (Package_Id) loop
               Format_Options_Builder.With_From_Attribute (Attribute);
            end loop;

            return Format_Options_Builder.Build;
         end;

      else
         Gnatformat_Trace.Trace ("Project does not have a Format package");

         return Undefined_Format_Options;
      end if;
   end From_Project;

   ---------
   -- Get --
   ---------

   function Get
     (Self    : in out Project_Format_Options_Cache_Type;
      Project : GPR2.Project.View.Object) return Format_Options_Type
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

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Optional_Unbounded_Strings;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback).Charset
        or Default_Basic_Format_Options.Charset.Value;
   end Get_Charset;

   ---------------------
   -- Get_End_Of_Line --
   ---------------------

   function Get_End_Of_Line
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return End_Of_Line_Kind
   is
      use Optional_End_Of_Line_Kinds;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback).End_Of_Line
        or Default_Basic_Format_Options.End_Of_Line.Value;
   end Get_End_Of_Line;

   ------------------------
   -- Get_Keyword_Casing --
   ------------------------

   function Get_Keyword_Casing
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return Keyword_Casing_Kind
   is
      use Optional_Keyword_Casing_Kinds;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback).Keyword_Casing
        or Default_Basic_Format_Options.Keyword_Casing.Value;
   end Get_Keyword_Casing;

   ----------------
   -- Get_Ignore --
   ----------------

   function Get_Ignore (Self : Format_Options_Type) return String_Hashed_Set
   is (if Self.Ignored_Sources.Is_Set
       then Self.Ignored_Sources.Value
       else String_Hashed_Sets.Empty_Set);

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
     (Self              : Format_Options_Type;
      Language_Fallback : Supported_Languages := Ada_Language)
      return Layout_Kind
   is (if Self.Language (Language_Fallback).Layout.Is_Set
       then Self.Language (Language_Fallback).Layout.Value
       else Default_Basic_Format_Options.Layout.Value);

   -------------------------
   -- Get_Override_Layout --
   -------------------------

   function Get_Override_Layout
     (Self              : Format_Options_Type;
      Language_Fallback : Supported_Languages := Ada_Language)
      return GNATCOLL.VFS.File_Array
   is
      function Files_Vector_To_Files_Array
        (FV : Files_Vector) return GNATCOLL.VFS.File_Array;
      --  Creates the file array based on the given file vector

      -----------------------------------
      --  Files_Vector_To_Files_Array  --
      -----------------------------------

      function Files_Vector_To_Files_Array
        (FV : Files_Vector) return GNATCOLL.VFS.File_Array
      is
         Res  : GNATCOLL.VFS.File_Array := GNATCOLL.VFS.Empty_File_Array;
         Last : Natural := 0;
      begin
         for F of FV loop
            Last := Last + 1;
            Res (Last) := F;
         end loop;

         return Res;
      end Files_Vector_To_Files_Array;

   begin
      return
        (if Self.Language (Language_Fallback).Override_Layout.Is_Set
         then
           Files_Vector_To_Files_Array
             (Self.Language (Language_Fallback).Override_Layout.Value)
         else GNATCOLL.VFS.Empty_File_Array);
   end Get_Override_Layout;

   ---------------------
   -- Get_Indentation --
   ---------------------

   function Get_Indentation
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language) return Positive
   is
      use Optional_Positives;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback).Indentation
        or Default_Basic_Format_Options.Indentation.Value;
   end Get_Indentation;

   ----------------------------------
   -- Get_Indentation_Continuation --
   ----------------------------------

   function Get_Indentation_Continuation
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language) return Positive
   is
      use Optional_Positives;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback)
          .Indentation_Continuation
        or Default_Basic_Format_Options.Indentation_Continuation.Value;
   end Get_Indentation_Continuation;

   --------------------------
   -- Get_Indentation_Kind --
   --------------------------

   function Get_Indentation_Kind
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return Indentation_Kind
   is
      use Optional_Indentation_Kinds;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback).Indentation_Kind
        or Default_Basic_Format_Options.Indentation_Kind.Value;
   end Get_Indentation_Kind;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language) return Positive
   is
      use Optional_Positives;

   begin
      return
        Into (Self, Source_Filename, Language_Fallback).Width
        or Default_Basic_Format_Options.Width.Value;
   end Get_Width;

   ----------
   -- Into --
   ----------

   function Into
     (Self : Basic_Format_Options_Type)
      return Prettier_Ada.Documents.Format_Options_Type
   is
      use type Optional_Positive;
      use Prettier_Ada.Documents;

      Width                    : constant Natural :=
        Self.Width or Default_Basic_Format_Options.Width.Value;
      Indentation              : constant Natural :=
        Self.Indentation or Default_Basic_Format_Options.Indentation.Value;
      Indentation_Kind         :
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
      Indentation_Continuation : constant Positive :=
        Self.Indentation_Continuation
        or Default_Basic_Format_Options.Indentation_Continuation.Value;
      Indentation_Offset       :
        constant Prettier_Ada.Documents.Indentation_Offset_Type :=
          (Tabs => 0, Spaces => 0);
      End_Of_Line              :
        constant Prettier_Ada.Documents.End_Of_Line_Kind :=
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
          (Width       => Width,
           Indentation =>
             (Indentation_Kind,
              Indentation,
              Indentation_Continuation,
              Indentation_Offset),
           End_Of_Line => End_Of_Line);
   end Into;

   ----------
   -- Into --
   ----------

   function Into
     (Self : Format_Options_Type; Language : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type
   is (Into (Self.Language (Language)));

   ----------
   -- Into --
   ----------

   function Into
     (Self : Format_Options_Type; Source_Filename : String)
      return Prettier_Ada.Documents.Format_Options_Type
   is (if Self.Sources.Contains (Source_Filename)
       then Into (Self.Sources.Element (Source_Filename))
       else Prettier_Ada.Documents.Default_Format_Options);

   -----------
   --  Into --
   -----------

   function Into
     (Format_Options    : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages) return Basic_Format_Options_Type
   is (if Format_Options.Sources.Contains (Source_Filename)
       then Format_Options.Sources.Element (Source_Filename)
       else Format_Options.Language (Language_Fallback));

   ----------
   -- Into --
   ----------

   function Into
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type
   is (if Self.Sources.Contains (Source_Filename)
       then Into (Self.Sources.Element (Source_Filename))
       else Into (Self, Language_Fallback));

   -----------------------------
   --  Load_Unparsing_Config  --
   -----------------------------

   function Load_Unparsing_Configuration
     (Unparsing_Configuration_File : GNATCOLL.VFS.Virtual_File;
      Overriddings                  : GNATCOLL.VFS.File_Array;
      Diagnostics                  :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration
   is
      use Langkit_Support.Generic_API;
   begin
      Gnatformat.Gnatformat_Trace.Trace
        ("Loading unparsing configuration from """
         & Unparsing_Configuration_File.Display_Full_Name
         & """");
      if Overriddings'Length > 0 then
         Gnatformat.Gnatformat_Trace.Trace ("With overridings: ");
         for O of Overriddings loop
            Gnatformat.Gnatformat_Trace.Trace (O.Display_Full_Name);
         end loop;
         Gnatformat.Gnatformat_Trace.Trace
           (Libadalang
              .Generic_API
              .Unparsing
              .Builtin_Overridings
              .Original_Keywords
              .Display_Full_Name);
      end if;

      return
        Unparsing.Load_Unparsing_Config
          (Libadalang.Generic_API.Ada_Lang_Id,
           Unparsing_Configuration_File.Display_Full_Name,
           Diagnostics,
           Overridings => Overriddings);
   end Load_Unparsing_Configuration;

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
      use type Optional_Unbounded_String;
      use type Optional_Keyword_Casing_Kind;
      use type Optional_Layout;
      use type Optional_Files_Vector;
   begin
      Target.Width := Source.Width or @;
      Target.Indentation := Source.Indentation or @;
      Target.Indentation_Continuation := Source.Indentation_Continuation or @;
      Target.Indentation_Kind := Source.Indentation_Kind or @;
      Target.End_Of_Line := Source.End_Of_Line or @;
      Target.Charset := Source.Charset or @;
      Target.Keyword_Casing := Source.Keyword_Casing or @;

      --  Handling rewrite of Layout and Override_Layout values
      Target.Layout := Source.Layout or @;
      Target.Override_Layout := Source.Override_Layout or @;
   end Overwrite;

   --------------
   -- Override --
   --------------

   procedure Overwrite
     (Target : in out Format_Options_Type; Source : Format_Options_Type)
   is
      use String_To_Basic_Format_Options_Hash_Maps;

      Source_Cursor : Cursor := Source.Sources.First;

   begin
      for Supported_Language in Supported_Languages loop
         Overwrite
           (Target.Language (Supported_Language),
            Source.Language (Supported_Language));
      end loop;

      while Has_Element (Source_Cursor) loop
         if Target.Sources.Contains (Key (Source_Cursor)) then
            Overwrite
              (Target.Sources.Reference (Source_Cursor),
               Element (Source_Cursor));

         else
            Target.Sources.Insert
              (Key (Source_Cursor), Element (Source_Cursor));
         end if;

         Next (Source_Cursor);
      end loop;

      if Source.Ignored_Sources.Is_Set then
         Target.Ignored_Sources := Source.Ignored_Sources;
      end if;
   end Overwrite;

   ------------------
   -- With_Charset --
   ------------------

   procedure With_Charset
     (Self     : in out Format_Options_Builder_Type;
      Charset  : Ada.Strings.Unbounded.Unbounded_String;
      Language : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).Charset :=
        (Is_Set => True, Value => Charset);
   end With_Charset;

   ------------------
   -- With_Charset --
   ------------------

   procedure With_Charset
     (Self            : in out Format_Options_Builder_Type;
      Charset         : Ada.Strings.Unbounded.Unbounded_String;
      Source_Filename : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).Charset :=
           (Is_Set => True, Value => Charset);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta Charset => (Is_Set => True, Value => Charset)));
      end if;
   end With_Charset;

   ----------------------
   -- With_End_Of_Line --
   ----------------------

   procedure With_End_Of_Line
     (Self            : in out Format_Options_Builder_Type;
      End_Of_Line     : End_Of_Line_Kind;
      Source_Filename : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).End_Of_Line :=
           (Is_Set => True, Value => End_Of_Line);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta
               End_Of_Line => (Is_Set => True, Value => End_Of_Line)));
      end if;
   end With_End_Of_Line;

   ----------------------
   -- With_End_Of_Line --
   ----------------------

   procedure With_End_Of_Line
     (Self        : in out Format_Options_Builder_Type;
      End_Of_Line : End_Of_Line_Kind;
      Language    : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).End_Of_Line :=
        (Is_Set => True, Value => End_Of_Line);
   end With_End_Of_Line;

   ------------------------
   -- With_Keyword_Casing --
   ------------------------

   procedure With_Keyword_Casing
     (Self           : in out Format_Options_Builder_Type;
      Keyword_Casing : Gnatformat.Configuration.Keyword_Casing_Kind;
      Language       : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).Keyword_Casing :=
        (Is_Set => True, Value => Keyword_Casing);
   end With_Keyword_Casing;

   ------------------------
   -- With_Keyword_Casing --
   ------------------------

   procedure With_Keyword_Casing
     (Self            : in out Format_Options_Builder_Type;
      Keyword_Casing  : Gnatformat.Configuration.Keyword_Casing_Kind;
      Source_Filename : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename)
           .Keyword_Casing :=
           (Is_Set => True, Value => Keyword_Casing);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta
               Keyword_Casing => (Is_Set => True, Value => Keyword_Casing)));
      end if;
   end With_Keyword_Casing;

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

      procedure Parse_Attribute (Attribute : GPR2.Project.Attribute.Object);
      --  Parses Attribute value by checking if it's one of the valid
      --  kinds and then converting to the expected value type. If parsed
      --  successfully, Self.Format_Options is updated accordingly.

      ---------------------
      -- Parse_Attribute --
      ---------------------

      procedure Parse_Attribute (Attribute : GPR2.Project.Attribute.Object) is
         use type GPR2.Q_Optional_Attribute_Id;

         type Format_Option_Kind is
           (Charset,
            End_Of_Line,
            Ignore,
            Indentation,
            Indentation_Kind,
            Indentation_Continuation,
            Keyword_Casing,
            Width,
            Layout,
            Override_Layout,
            Unknown);

         type Attribute_Value_Type (Kind : Format_Option_Kind := Unknown) is
         record
            case Kind is
               when Charset =>
                  Charset : Ada.Strings.Unbounded.Unbounded_String;

               when End_Of_Line =>
                  End_Of_Line : End_Of_Line_Kind;

               when Ignore =>
                  Ignore : Ada.Strings.Unbounded.Unbounded_String;

               when Indentation =>
                  Indentation : Positive;

               when Indentation_Continuation =>
                  Indentation_Continuation : Positive;

               when Indentation_Kind =>
                  Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;

               when Keyword_Casing =>
                  Keyword_Casing :
                    Gnatformat.Configuration.Keyword_Casing_Kind;

               when Width =>
                  Width : Positive;

               when Layout =>
                  Layout : Gnatformat.Configuration.Layout_Kind;

               when Override_Layout =>
                  Override_Layout : Optional_Files_Vector;

               when Unknown =>
                  null;
            end case;
         end record;

         function Parse_Attribute_Value return Attribute_Value_Type;
         --  Parses Attribute value by checking if it's one of the valid
         --  kinds and then converting to the expected value type.

         ---------------------------
         -- Parse_Attribute_Value --
         ---------------------------

         function Parse_Attribute_Value return Attribute_Value_Type is
            Q_Attribute_Id : constant GPR2.Q_Optional_Attribute_Id :=
              Attribute.Name.Id;

            function To_Optional_Files_Vector
              (Values : GPR2.Containers.Source_Value_List)
               return Gnatformat.Configuration.Optional_Files_Vector;
            --  Converts GPR2.Containers.Source_Value_List to
            --  Gnatformat.Configuration.Optional_Files_Vector.

            --------------------------------
            --  To_Optional_Files_Vector  --
            --------------------------------

            function To_Optional_Files_Vector
              (Values : GPR2.Containers.Source_Value_List)
               return Gnatformat.Configuration.Optional_Files_Vector
            is
               use type GPR2.Containers.Source_Value_Type_List.Vector;

               function To_Virtual_File
                 (File_Name : String) return GNATCOLL.VFS.Virtual_File
               is (GNATCOLL.VFS.Create_From_UTF8
                     (File_Name, Normalize => False));

            begin
               if Values /= GPR2.Containers.Empty_Source_Value_List then
                  return Optional_Files_Vector'(Is_Set => False);
               end if;

               begin
                  return
                    Optional_Files_Vector'
                      (Is_Set => True,
                       Value  =>
                         [for V of Values => To_Virtual_File (V.Text)]);
               exception
                  when others =>
                     Gnatformat_Trace.Trace ("Virtual file creation failed");
                     return Optional_Files_Vector'(Is_Set => False);
               end;

            end To_Optional_Files_Vector;

            Raw_Attribute_Value : constant String :=
              (if Attribute.Kind = GPR2.Project.Registry.Attribute.Single
               then Attribute.Value.Text
               else "");
         begin
            if Q_Attribute_Id = Q_Charset_Attribute_Id then
               return
                 (Kind    => Charset,
                  Charset =>
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Width_Attribute_Id then
               return
                 (Kind  => Width,
                  Width => Positive'Value (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Indentation_Attribute_Id then
               return
                 (Kind        => Indentation,
                  Indentation => Positive'Value (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Indentation_Continuation_Attribute_Id then
               return
                 (Kind                     => Indentation_Continuation,
                  Indentation_Continuation =>
                    Positive'Value (Raw_Attribute_Value));

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

            elsif Q_Attribute_Id = Q_Keyword_Casing_Attribute_Id then
               return
                 (Kind           => Keyword_Casing,
                  Keyword_Casing =>
                    Gnatformat.Configuration.Keyword_Casing_Kind'Value
                      (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Ignore_Attribute_Id then
               return
                 (Kind   => Ignore,
                  Ignore =>
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Layout_Attribute_Id then
               return
                 (Kind   => Layout,
                  Layout => Layout_Kind'Value (Raw_Attribute_Value));

            elsif Q_Attribute_Id = Q_Override_Layout_Attribute_Id then
               return
                 (Kind            => Override_Layout,
                  Override_Layout =>
                    To_Optional_Files_Vector (Attribute.Values));

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

         if not Attribute.Index.Is_Defined then
            case Attribute_Value.Kind is
               when Ignore  =>
                  Gnatformat_Trace.Trace
                    (Ignore'Image
                     & " = "
                     & Ada.Strings.Unbounded.To_String
                         (Attribute_Value.Ignore));
                  Self.With_Ignore
                    (GNATCOLL.VFS.Create_From_UTF8
                       (Ada.Strings.Unbounded.To_String
                          (Attribute_Value.Ignore)));

               when Unknown =>
                  Gnatformat_Trace.Trace ("Unknown attribute");

               when others  =>
                  Gnatformat_Trace.Trace
                    (Attribute_Value.Kind'Image
                     & " attribute must have an index");
            end case;

         elsif Attribute.Index = Ada_Attribute_Index then
            Gnatformat_Trace.Trace ("Ada attribute " & Attribute.Index.Text);

            case Attribute_Value.Kind is
               when Charset                  =>
                  Gnatformat_Trace.Trace
                    (Charset'Image
                     & " = "
                     & Ada.Strings.Unbounded.To_String
                         (Attribute_Value.Charset));
                  Self.With_Charset (Attribute_Value.Charset, Ada_Language);

               when End_Of_Line              =>
                  Gnatformat_Trace.Trace
                    (End_Of_Line'Image
                     & " = "
                     & Attribute_Value.End_Of_Line'Image);
                  Self.With_End_Of_Line
                    (Attribute_Value.End_Of_Line, Ada_Language);

               when Indentation              =>
                  Gnatformat_Trace.Trace
                    (Indentation'Image
                     & " = "
                     & Attribute_Value.Indentation'Image);

                  Self.With_Indentation
                    (Attribute_Value.Indentation, Ada_Language);

               when Indentation_Continuation =>
                  Gnatformat_Trace.Trace
                    (Indentation_Continuation'Image
                     & " = "
                     & Attribute_Value.Indentation_Continuation'Image);
                  Self.With_Indentation_Continuation
                    (Attribute_Value.Indentation_Continuation, Ada_Language);

               when Indentation_Kind         =>
                  Gnatformat_Trace.Trace
                    (Indentation_Kind'Image
                     & " = "
                     & Attribute_Value.Indentation_Kind'Image);
                  Self.With_Indentation_Kind
                    (Attribute_Value.Indentation_Kind, Ada_Language);

               when Keyword_Casing           =>
                  Gnatformat_Trace.Trace
                    (Keyword_Casing'Image
                     & " = "
                     & Attribute_Value.Keyword_Casing'Image);
                  Self.With_Keyword_Casing
                    (Attribute_Value.Keyword_Casing, Ada_Language);

               when Width                    =>
                  Gnatformat_Trace.Trace
                    (Width'Image & " = " & Attribute_Value.Width'Image);
                  Self.With_Width (Attribute_Value.Width, Ada_Language);

               when Ignore                   =>
                  Gnatformat_Trace.Trace
                    (Ignore'Image & " attribute must not have an index");

               when Layout                   =>
                  Gnatformat_Trace.Trace
                    (Layout'Image & " = " & Attribute_Value.Layout'Image);
                  Self.With_Layout (Attribute_Value.Layout, Ada_Language);

               when Override_Layout          =>
                  Gnatformat_Trace.Trace
                    (Override_Layout'Image
                     & " = "
                     & Attribute_Value.Override_Layout'Image);
                  Self.With_Override_Layout
                    (Attribute_Value.Override_Layout, Ada_Language);

               when Unknown                  =>
                  Gnatformat_Trace.Trace ("Unknown attribute");
            end case;

         else
            Gnatformat_Trace.Trace
              ("Source attribute for " & Attribute.Index.Text);

            case Attribute_Value.Kind is
               when Charset                  =>
                  Gnatformat_Trace.Trace
                    (Charset'Image
                     & " = "
                     & Ada.Strings.Unbounded.To_String
                         (Attribute_Value.Charset));
                  Self.With_Charset
                    (Attribute_Value.Charset, Attribute.Index.Text);

               when End_Of_Line              =>
                  Gnatformat_Trace.Trace
                    (End_Of_Line'Image
                     & " = "
                     & Attribute_Value.End_Of_Line'Image);
                  Self.With_End_Of_Line
                    (Attribute_Value.End_Of_Line, Attribute.Index.Text);

               when Indentation              =>
                  Gnatformat_Trace.Trace
                    (Indentation'Image
                     & " = "
                     & Attribute_Value.Indentation'Image);
                  Self.With_Indentation
                    (Attribute_Value.Indentation, Attribute.Index.Text);

               when Indentation_Continuation =>
                  Gnatformat_Trace.Trace
                    (Indentation_Continuation'Image
                     & " = "
                     & Attribute_Value.Indentation_Continuation'Image);
                  Self.With_Indentation_Continuation
                    (Attribute_Value.Indentation_Continuation,
                     Attribute.Index.Text);

               when Indentation_Kind         =>
                  Gnatformat_Trace.Trace
                    (Indentation_Kind'Image
                     & " = "
                     & Attribute_Value.Indentation_Kind'Image);
                  Self.With_Indentation_Kind
                    (Attribute_Value.Indentation_Kind, Attribute.Index.Text);

               when Keyword_Casing           =>
                  Gnatformat_Trace.Trace
                    (Keyword_Casing'Image
                     & " = "
                     & Attribute_Value.Keyword_Casing'Image);
                  Self.With_Keyword_Casing
                    (Attribute_Value.Keyword_Casing, Attribute.Index.Text);

               when Width                    =>
                  Gnatformat_Trace.Trace
                    (Width'Image & " = " & Attribute_Value.Width'Image);
                  Self.With_Width
                    (Attribute_Value.Width, Attribute.Index.Text);

               when Ignore                   =>
                  Gnatformat_Trace.Trace
                    (Ignore'Image & " attribute must not have an index");

               when Layout                   =>
                  Gnatformat_Trace.Trace
                    (Layout'Image & " = " & Attribute_Value.Layout'Image);
                  Self.With_Layout (Attribute_Value.Layout, Ada_Language);

               when Override_Layout          =>
                  Gnatformat_Trace.Trace
                    (Override_Layout'Image
                     & " = "
                     & Attribute_Value.Override_Layout'Image);
                  Self.With_Override_Layout
                    (Attribute_Value.Override_Layout, Ada_Language);

               when Unknown                  =>
                  Gnatformat_Trace.Trace ("Unknown attribute");
            end case;
         end if;
      end Parse_Attribute;

   begin
      Gnatformat_Trace.Trace ("Parsing attribute """ & Attribute.Image & """");

      if Attribute.Has_Index then
         case Attribute.Kind is
            when GPR2.Project.Registry.Attribute.Single =>
               Parse_Attribute (Attribute);

            when GPR2.Project.Registry.Attribute.List   =>
               Gnatformat_Trace.Trace
                 ("Attribute is a list, not a single value");
               Parse_Attribute (Attribute);
         end case;

      else
         Parse_Attribute (Attribute);
      end if;
   end With_From_Attribute;

   -----------------
   -- With_Ignore --
   -----------------

   procedure With_Ignore
     (Self   : in out Format_Options_Builder_Type;
      Ignore : GNATCOLL.VFS.Virtual_File)
   is

      function Is_Not_Comment_Line (Str : String) return Boolean
      is (Str'Length /= 0 and then Str (Str'First .. Str'First + 2) /= "-- ");

   begin
      declare
         Resolved_Ignore : constant GNATCOLL.VFS.Virtual_File :=
           (if Self.Project.Is_Set
            then
              (declare
                 Project_File : constant GNATCOLL.VFS.Virtual_File :=
                   GNATCOLL.VFS.Create_From_UTF8
                     (Self.Project.Value.Path_Name.String_Value);
               begin
                 GNATCOLL.VFS.Join (Project_File.Dir, Ignore))
            else Ignore);

      begin
         Self.Format_Options.Ignored_Sources :=
           (Is_Set => True, Value => String_Hashed_Sets.Empty_Set);

         if not Resolved_Ignore.Is_Regular_File then
            return;
         end if;

         declare
            Ignore_File : Ada.Text_IO.File_Type;

         begin
            Ada.Text_IO.Open
              (File => Ignore_File,
               Mode => Ada.Text_IO.In_File,
               Name => Resolved_Ignore.Display_Full_Name);

            while not Ada.Text_IO.End_Of_File (Ignore_File) loop
               declare
                  Current_Line : constant String :=
                    Ada.Strings.Unbounded.To_String
                      (Ada.Text_IO.Unbounded_IO.Get_Line (Ignore_File));
               begin
                  if Is_Not_Comment_Line (Current_Line) then
                     Self.Format_Options.Ignored_Sources.Value.Include
                       (Current_Line);
                  end if;
               end;

            end loop;

            Ada.Text_IO.Close (Ignore_File);
         end;
      end;

   exception
      when E : others =>
         Gnatformat_Trace.Trace (Ada.Exceptions.Exception_Information (E));
         Gnatformat_Trace.Trace
           (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

         Self.Format_Options.Ignored_Sources :=
           (Is_Set => True, Value => String_Hashed_Sets.Empty_Set);
   end With_Ignore;

   -----------------
   -- With_Layout --
   -----------------

   procedure With_Layout
     (Self     : in out Format_Options_Builder_Type;
      Layout   : Layout_Kind;
      Language : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).Layout :=
        (Is_Set => True, Value => Layout);
   end With_Layout;

   -----------------
   -- With_Layout --
   -----------------
   procedure With_Layout
     (Self            : in out Format_Options_Builder_Type;
      Layout          : Layout_Kind;
      Source_Filename : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).Layout :=
           (Is_Set => True, Value => Layout);
      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta Layout => (Is_Set => True, Value => Layout)));
      end if;
   end With_Layout;

   --------------------------
   -- With_Override_Layout --
   --------------------------

   procedure With_Override_Layout
     (Self                  : in out Format_Options_Builder_Type;
      Override_Layout_Files : Optional_Files_Vector;
      Language              : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).Override_Layout :=
        Override_Layout_Files;
   end With_Override_Layout;

   --------------------------
   -- With_Override_Layout --
   --------------------------

   procedure With_Override_Layout
     (Self                  : in out Format_Options_Builder_Type;
      Override_Layout_Files : Optional_Files_Vector;
      Source_Filename       : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename)
           .Override_Layout :=
           Override_Layout_Files;
      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta Override_Layout => Override_Layout_Files));
      end if;
   end With_Override_Layout;

   ----------------------
   -- With_Indentation --
   ----------------------

   procedure With_Indentation
     (Self            : in out Format_Options_Builder_Type;
      Indentation     : Positive;
      Source_Filename : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).Indentation :=
           (Is_Set => True, Value => Indentation);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta
               Indentation => (Is_Set => True, Value => Indentation)));
      end if;

      if Self.Implicit_Indentation_Continuation then
         if Self.Format_Options.Sources.Contains (Source_Filename) then
            if not Self.Format_Options.Sources.Reference (Source_Filename)
                     .Indentation_Continuation
                     .Is_Set
            then
               Self.Format_Options.Sources.Reference (Source_Filename)
                 .Indentation_Continuation :=
                 (Is_Set => True,
                  Value  => (if Indentation = 1 then 1 else Indentation - 1));
            end if;
         else
            Self.Format_Options.Sources.Insert
              (Source_Filename,
               (Undefined_Basic_Format_Options
                with delta
                  Indentation_Continuation =>
                    (Is_Set => True,
                     Value  =>
                       (if Indentation = 1 then 1 else Indentation - 1))));
         end if;
      end if;
   end With_Indentation;

   ----------------------
   -- With_Indentation --
   ----------------------

   procedure With_Indentation
     (Self        : in out Format_Options_Builder_Type;
      Indentation : Positive;
      Language    : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).Indentation :=
        (Is_Set => True, Value => Indentation);

      if Self.Implicit_Indentation_Continuation
        --  Avoid overwriting
        and
          not Self.Format_Options.Language (Language)
                .Indentation_Continuation
                .Is_Set
      then
         Self.Format_Options.Language (Language).Indentation_Continuation :=
           (Is_Set => True,
            Value  => (if Indentation = 1 then 1 else Indentation - 1));
      end if;
   end With_Indentation;

   -----------------------------------
   -- With_Indentation_Continuation --
   -----------------------------------

   procedure With_Indentation_Continuation
     (Self                     : in out Format_Options_Builder_Type;
      Indentation_Continuation : Positive;
      Language                 : Supported_Languages) is
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
      Source_Filename          : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename)
           .Indentation_Continuation :=
           (Is_Set => True, Value => Indentation_Continuation);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta
               Indentation_Continuation =>
                 (Is_Set => True, Value => Indentation_Continuation)));
      end if;
   end With_Indentation_Continuation;

   ---------------------------
   -- With_Indentation_Kind --
   ---------------------------

   procedure With_Indentation_Kind
     (Self             : in out Format_Options_Builder_Type;
      Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
      Source_Filename  : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename)
           .Indentation_Kind :=
           (Is_Set => True, Value => Indentation_Kind);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta
               Indentation_Kind =>
                 (Is_Set => True, Value => Indentation_Kind)));
      end if;
   end With_Indentation_Kind;

   ---------------------------
   -- With_Indentation_Kind --
   ---------------------------

   procedure With_Indentation_Kind
     (Self             : in out Format_Options_Builder_Type;
      Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
      Language         : Supported_Languages) is
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
      Source_Filename : String) is
   begin
      if Self.Format_Options.Sources.Contains (Source_Filename) then
         Self.Format_Options.Sources.Reference (Source_Filename).Width :=
           (Is_Set => True, Value => Width);

      else
         Self.Format_Options.Sources.Insert
           (Source_Filename,
            (Undefined_Basic_Format_Options
             with delta Width => (Is_Set => True, Value => Width)));
      end if;
   end With_Width;

   ----------------
   -- With_Width --
   ----------------

   procedure With_Width
     (Self     : in out Format_Options_Builder_Type;
      Width    : Positive;
      Language : Supported_Languages) is
   begin
      Self.Format_Options.Language (Language).Width :=
        (Is_Set => True, Value => Width);
   end With_Width;

   ------------------------------------------
   -- Create_Unparsing_Configuration_Cache --
   ------------------------------------------

   function Create_Unparsing_Configuration_Cache
     (Default_Unparsing_Configuration : GNATCOLL.VFS.Virtual_File)
      return Unparsing_Configuration_Cache_Type
   is (Unparsing_Configuration_Cache_Type'
         (Default               => Default_Unparsing_Configuration,
          Cache                 =>
            View_Ids_To_Unparsing_Config_Hashed_Maps.Empty_Map,
          No_Project            => <>,
          Initialize_No_Project => True));

   ---------
   -- Get --
   ---------

   function Get
     (Self            : in out Unparsing_Configuration_Cache_Type;
      Source_Filename : String;
      Project         : GPR2.Project.View.Object;
      Format_Options  : Format_Options_Type;
      Diagnostics     :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration
   is
      Id : constant GPR2.View_Ids.View_Id := Project.Id;

   begin
      --  Check if this project view already has a cached unparsing
      --  configuration.

      if Self.Cache.Contains (Id) then
         --  Use cache for this source if existent

         if Self.Cache.Constant_Reference (Id).Sources.Contains
              (Source_Filename)
         then
            return
              Self.Cache.Constant_Reference (Id).Sources.Element
                (Source_Filename);
         end if;

         --  No cache for this source.
         --  Check if has custom format options.

         if Format_Options.Sources.Contains (Source_Filename) then
            --  Source specific unparsing configuration are computed on
            --  demand since there's no guarantee they will ever be used.

            declare
               Source_Format_Options          :
                 constant Basic_Format_Options_Type :=
                   Format_Options.Sources.Element (Source_Filename);
               Source_Unparsing_Configuration :
                 constant Langkit_Support
                            .Generic_API
                            .Unparsing
                            .Unparsing_Configuration :=
                   Compute_Unparsing_Configuration
                     (Source_Format_Options, Self.Default, Diagnostics);

            begin
               Self.Cache.Reference (Id).Sources.Include
                 (Source_Filename, Source_Unparsing_Configuration);
               return Source_Unparsing_Configuration;
            end;
         end if;

         --  No custom format options. Fallback to language defaults.

         return Self.Cache.Constant_Reference (Id).Language (Ada_Language);
      end if;

      --  This project view does not have an unparsing configuration yet.
      --  Compute one. Source specific unparsing configuration are computed on
      --  demand since there's no guarantee they will ever be used.

      declare
         Unparsing_Configuration : constant Unparsing_Configuration_Type :=
           (Language =>
              [Ada_Language =>
                 Compute_Unparsing_Configuration
                   (Format_Options                  =>
                      Format_Options.Language (Ada_Language),
                    Default_Unparsing_Configuration => Self.Default,
                    Diagnostics                     => Diagnostics)],
            Sources  =>
              String_To_Basic_Unparsing_Configuration_Hash_Maps.Empty_Map);

      begin
         Self.Cache.Include (Id, Unparsing_Configuration);

         --  At this point, this project unparsing configuration is cached.
         --  Recurse.

         return
           Self.Get (Source_Filename, Project, Format_Options, Diagnostics);
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self            : in out Unparsing_Configuration_Cache_Type;
      Source_Filename : String;
      Format_Options  : Format_Options_Type;
      Diagnostics     :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration is
   begin
      --  Try to get a cached unparsing configuration

      if Self.No_Project.Sources.Contains (Source_Filename) then
         return Self.No_Project.Sources.Element (Source_Filename);
      end if;

      --  No cache for this source.
      --  Check if has custom format options.

      if Format_Options.Sources.Contains (Source_Filename) then
         --  Source specific unparsing configurations are computed on
         --  demand since there's no guarantee they will ever be used.

         declare
            Source_Format_Options          :
              constant Basic_Format_Options_Type :=
                Format_Options.Sources.Element (Source_Filename);
            Source_Unparsing_Configuration :
              constant Langkit_Support
                         .Generic_API
                         .Unparsing
                         .Unparsing_Configuration :=
                Compute_Unparsing_Configuration
                  (Source_Format_Options, Self.Default, Diagnostics);

         begin
            Self.No_Project.Sources.Include
              (Source_Filename, Source_Unparsing_Configuration);

            return Source_Unparsing_Configuration;
         end;
      end if;

      --  Language specific unparsing configurations are computed on demand

      if Self.Initialize_No_Project then
         Self.Initialize_No_Project := False;
         Self.No_Project.Language :=
           [Compute_Unparsing_Configuration
              (Format_Options.Language (Ada_Language),
               Self.Default,
               Diagnostics)];
      end if;

      return Self.No_Project.Language (Ada_Language);
   end Get;

end Gnatformat.Configuration;
