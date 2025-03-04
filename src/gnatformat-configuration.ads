--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with Gnatformat.Utils;

with GNATCOLL.VFS;

with GPR2.Project.View;
with GPR2.Project.Attribute;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Prettier_Ada.Documents;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Indefinite_Hashed_Maps;

private with GPR2.View_Ids;

private with Libadalang.Generic_API;

package Gnatformat.Configuration is

   Default_Charset : constant String := "iso-8859-1";

   type Format_Options_Type;

   procedure Elaborate_GPR2;
   --  Updates the GPR2 registry with the following:
   --
   --     - package Format
   --     - atribute Width, Indentation, Indentation_Kind,
   --       Indentation_Continuation, End_Of_Line and Charset
   --       indexed by language or source.

   type Indentation_Kind is (Spaces, Tabs);
   type End_Of_Line_Kind is (LF, CRLF);

   package Optional_Indentation_Kinds is new
     Gnatformat.Utils.Optional (Indentation_Kind);
   subtype Optional_Indentation_Kind is
     Optional_Indentation_Kinds.Optional_Type;

   package Optional_End_Of_Line_Kinds is new
     Gnatformat.Utils.Optional (End_Of_Line_Kind);
   subtype Optional_End_Of_Line_Kind is
     Optional_End_Of_Line_Kinds.Optional_Type;

   package Optional_Positives is new Gnatformat.Utils.Optional (Positive);
   subtype Optional_Positive is Optional_Positives.Optional_Type;

   package Optional_Unbounded_Strings is new
     Gnatformat.Utils.Optional (Ada.Strings.Unbounded.Unbounded_String);
   subtype Optional_Unbounded_String is
     Optional_Unbounded_Strings.Optional_Type;

   package String_Hashed_Sets is new
     Ada.Containers.Indefinite_Hashed_Sets (String, Ada.Strings.Hash, "=");

   subtype String_Hashed_Set is String_Hashed_Sets.Set;

   package Optional_String_Hashed_Sets is new
     Gnatformat.Utils.Optional (String_Hashed_Set);
   subtype Optional_String_Hashed_Set is
     Optional_String_Hashed_Sets.Optional_Type;

   type Format_Options_Type is private;

   function Get_Charset
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Retrieves the charset option for the specified Source_Filename if it
   --  exists.
   --  If the charset option for Source_Filename does not exist, the function
   --  will fall back to the charset option associated with the given
   --  Language_Fallback if it is available.
   --  If neither the Source_Filename charset option nor the Language_Fallback
   --  option is available, the function returns the default charset defined
   --  by Default_Basic_Format_Options.Charset.Value.

   function Get_End_Of_Line
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return End_Of_Line_Kind;
   --  Retrieves the end-of-line option for the specified Source_Filename if it
   --  exists.
   --  If the end-of-line option for Source_Filename does not exist, the
   --  function will fall back to the end-of-line option associated with the
   --  given Language_Fallback if it is available.
   --  If neither the Source_Filename end-of-line option nor the
   --  Language_Fallback option is available, the function returns the default
   --  end-of-line value defined by
   --  Default_Basic_Format_Options.End_Of_Line.Value.

   function Get_Ignore (Self : Format_Options_Type) return String_Hashed_Set;
   --  Retrieves the already resolved ignore option, i.e., the contents (list
   --  of sources to be ignored) of the file used to build the ignore option.
   --  Defaults to an empty String_Hashed_Set is Format_Options_Type was not
   --  built with an ignore option.

   function Get_Indentation
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language) return Positive;
   --  Retrieves the indentation option for the specified Source_Filename if it
   --  exists.
   --  If the indentation option for Source_Filename does not exist, the
   --  function will fall back to the indentation option associated with the
   --  given Language_Fallback if it is available.
   --  If neither the Source_Filename indentation option nor the
   --  Language_Fallback option is available, the function returns the default
   --  indentation value defined by
   --  Default_Basic_Format_Options.End_Of_Line.Value.

   function Get_Indentation_Kind
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language)
      return Indentation_Kind;
   --  Retrieves the indentaiton-kind option for the specified Source_Filename
   --  if it exists.
   --  If the indentaiton-kind option for Source_Filename does not exist, the
   --  function will fall back to the indentaiton-kind option associated with
   --  the given Language_Fallback if it is available.
   --  If neither the Source_Filename indentaiton-kind option nor the
   --  Language_Fallback option is available, the function returns the default
   --  indentaiton-kind value defined by
   --  Default_Basic_Format_Options.End_Of_Line.Value.

   function Get_Indentation_Continuation
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language) return Positive;
   --  Retrieves the indentaiton-continuation option for the specified
   --  Source_Filename it exists.
   --  If the indentaiton-continuation option for Source_Filename does not
   --  exist, the function will fall back to the indentaiton-continuation
   --  option associated with the given Language_Fallback if it is available.
   --  If neither the Source_Filename indentaiton-continuation option nor the
   --  Language_Fallback option is available, the function returns the default
   --  indentaiton-continuation value defined by
   --  Default_Basic_Format_Options.End_Of_Line.Value.

   function Get_Width
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages := Ada_Language) return Positive;
   --  Retrieves the width option for the specified Source_Filename if it
   --  exists.
   --  If the width option for Source_Filename does not exist, the
   --  function will fall back to the width option associated with the
   --  given Language_Fallback if it is available.
   --  If neither the Source_Filename width option nor the
   --  Language_Fallback option is available, the function returns the default
   --  width value defined by
   --  Default_Basic_Format_Options.End_Of_Line.Value.

   function From_Project
     (Project : GPR2.Project.View.Object) return Format_Options_Type;
   --  Creates a Format_Options_Type by parsing Project's Format package

   function Into
     (Self : Format_Options_Type; Language : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Converts a Format_Options_type for the provided Language into an
   --  equivalent Prettier_Ada Format_Options.Type. If there's no configuration
   --  associated to Language, then returns Default_Format_Options.

   function Into
     (Self : Format_Options_Type; Source_Filename : String)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Converts a Format_Options_type for the provided Source_Filename into an
   --  equivalent Prettier_Ada Format_Options.Type. If there's no configuration
   --  associated to Source_Filename, then returns Default_Format_Options.

   function Into
     (Self              : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Converts a Format_Options_type for the provided Source_Filename into an
   --  equivalent Prettier_Ada Format_Options.Type. If there's no configuration
   --  for this source, then fallback to the configuration associated to
   --  Langauge_Fallback. If there's no configuration for Language_Fallback
   --  returns Default_Format_Options.

   procedure Overwrite
     (Target : in out Format_Options_Type; Source : Format_Options_Type);
   --  Overwrites Target's options by Source's ones

   Default_Format_Options : constant Format_Options_Type;

   type Project_Format_Options_Cache_Type is private;

   function Create_Project_Format_Options_Cache
      return Project_Format_Options_Cache_Type;
   --  Project_Format_Options_Cache_Type constructor.
   --
   --  Use this cache if you need to repeatedly retrive format options for the
   --  same project view.

   function Get
     (Self    : in out Project_Format_Options_Cache_Type;
      Project : GPR2.Project.View.Object) return Format_Options_Type;
   --  Retrives the format options associated to Project using a cached result
   --  if existent. Otherwise the format options are computed.

   type Format_Options_Builder_Type is tagged private;
   --  Note: this is not a lazy builder. Each With_<Option> method will
   --  actively build the inner Format_Options_Type.

   package Optional_GPR2_Project_Views is new
     Gnatformat.Utils.Optional (GPR2.Project.View.Object);

   subtype Optional_GPR2_Project_View is
     Optional_GPR2_Project_Views.Optional_Type;

   function Create_Format_Options_Builder
     (Project                           : Optional_GPR2_Project_View :=
        (Is_Set => False);
      Implicit_Indentation_Continuation : Boolean := True)
      return Format_Options_Builder_Type;
   --  Format_Options_Builder_Type constructor.
   --
   --  A builder of Format_Options_Type.

   function Build
     (Self : Format_Options_Builder_Type) return Format_Options_Type;
   --  Returns the format options current state

   procedure With_Charset
     (Self     : in out Format_Options_Builder_Type;
      Charset  : Ada.Strings.Unbounded.Unbounded_String;
      Language : Supported_Languages);
   --  Sets the format option Charset for the provided Language

   procedure With_Charset
     (Self            : in out Format_Options_Builder_Type;
      Charset         : Ada.Strings.Unbounded.Unbounded_String;
      Source_Filename : String);
   --  Sets the format option Charset for the provided Source_Filename

   procedure With_End_Of_Line
     (Self        : in out Format_Options_Builder_Type;
      End_Of_Line : End_Of_Line_Kind;
      Language    : Supported_Languages);
   --  Sets the format option End_Of_Line for the provided Language

   procedure With_End_Of_Line
     (Self            : in out Format_Options_Builder_Type;
      End_Of_Line     : End_Of_Line_Kind;
      Source_Filename : String);
   --  Sets the format option End_Of_Line for the provided Source_Filename

   procedure With_From_Attribute
     (Self      : in out Format_Options_Builder_Type;
      Attribute : GPR2.Project.Attribute.Object);
   --  Parses Attribute and sets a format option according to the Attribute id
   --  and value. If the Attribute id is unknown, then it's ignored.

   procedure With_Ignore
     (Self   : in out Format_Options_Builder_Type;
      Ignore : GNATCOLL.VFS.Virtual_File);
   --  Sets the Ignore option

   procedure With_Indentation
     (Self        : in out Format_Options_Builder_Type;
      Indentation : Positive;
      Language    : Supported_Languages);
   --  Sets the format option Indentation for the provided Language

   procedure With_Indentation
     (Self            : in out Format_Options_Builder_Type;
      Indentation     : Positive;
      Source_Filename : String);
   --  Sets the format option Indentation for the provided Source_Filename

   procedure With_Indentation_Continuation
     (Self                     : in out Format_Options_Builder_Type;
      Indentation_Continuation : Positive;
      Language                 : Supported_Languages);
   --  Sets the format option Indentation_Continuation for the provided
   --  Language.

   procedure With_Indentation_Continuation
     (Self                     : in out Format_Options_Builder_Type;
      Indentation_Continuation : Positive;
      Source_Filename          : String);
   --  Sets the format option Indentation_Continuation for the provided
   --  Source_Filename.

   procedure With_Indentation_Kind
     (Self             : in out Format_Options_Builder_Type;
      Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
      Language         : Supported_Languages);
   --  Sets the format option Indentation_Kind for the provided Language

   procedure With_Indentation_Kind
     (Self             : in out Format_Options_Builder_Type;
      Indentation_Kind : Gnatformat.Configuration.Indentation_Kind;
      Source_Filename  : String);
   --  Sets the format option Indentation_Kind for the provided Source_Filename

   procedure With_Width
     (Self     : in out Format_Options_Builder_Type;
      Width    : Positive;
      Language : Supported_Languages);
   --  Sets the format option Width for the provided Language

   procedure With_Width
     (Self            : in out Format_Options_Builder_Type;
      Width           : Positive;
      Source_Filename : String);
   --  Sets the format option Width for the provided Source_Filename

   Default_Unparsing_Configuration :
     constant Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;

   function Load_Unparsing_Configuration
     (Unparsing_Configuration_File : GNATCOLL.VFS.Virtual_File;
      Diagnostics                  :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
   --  Loads the formatting rules

private

   Package_Id : constant GPR2.Package_Id := GPR2."+" ("format");

   Width_Attribute_Id   : constant GPR2.Attribute_Id := GPR2."+" ("width");
   Q_Width_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Width_Attribute_Id);

   Ignore_Attribute_Id   : constant GPR2.Attribute_Id := GPR2."+" ("ignore");
   Q_Ignore_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Ignore_Attribute_Id);

   Indentation_Attribute_Id   : constant GPR2.Attribute_Id :=
     GPR2."+" ("indentation");
   Q_Indentation_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Indentation_Attribute_Id);

   Indentation_Kind_Attribute_Id   : constant GPR2.Attribute_Id :=
     GPR2."+" ("indentation_kind");
   Q_Indentation_Kind_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Indentation_Kind_Attribute_Id);

   Indentation_Continuation_Attribute_Id   : constant GPR2.Attribute_Id :=
     GPR2."+" ("indentation_continuation");
   Q_Indentation_Continuation_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Indentation_Continuation_Attribute_Id);

   End_Of_Line_Attribute_Id   : constant GPR2.Attribute_Id :=
     GPR2."+" ("end_of_line");
   Q_End_Of_Line_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, End_Of_Line_Attribute_Id);

   Charset_Attribute_Id   : constant GPR2.Attribute_Id := GPR2."+" ("charset");
   Q_Charset_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Charset_Attribute_Id);

   type Basic_Format_Options_Type is record
      Width                    : Optional_Positive := (Is_Set => False);
      Indentation              : Optional_Positive := (Is_Set => False);
      Indentation_Kind         : Optional_Indentation_Kind :=
        (Is_Set => False);
      Indentation_Continuation : Optional_Positive := (Is_Set => False);
      End_Of_Line              : Optional_End_Of_Line_Kind :=
        (Is_Set => False);
      Charset                  : Optional_Unbounded_String :=
        (Is_Set => False);
   end record;

   function Into
     (Self : Basic_Format_Options_Type)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Converts a Basic_Format_Options_Type into an equivalent
   --  Prettier_Ada Format_Options.Type.

   function Into
     (Format_Options    : Format_Options_Type;
      Source_Filename   : String;
      Language_Fallback : Supported_Languages)
      return Basic_Format_Options_Type;
   --  Gets Basic_Format_Options_Type for Source_Filename if existent,
   --  otherwise fallsback Language_Fallback's Basic_Format_Options_Type.

   procedure Overwrite
     (Target : in out Basic_Format_Options_Type;
      Source : Basic_Format_Options_Type);
   --  Overwrites Target's options by Source's ones

   Default_Basic_Format_Options : constant Basic_Format_Options_Type :=
     (Width                    => (Is_Set => True, Value => 79),
      Indentation              => (Is_Set => True, Value => 3),
      Indentation_Kind         => (Is_Set => True, Value => Spaces),
      Indentation_Continuation => (Is_Set => True, Value => 2),
      End_Of_Line              => (Is_Set => True, Value => LF),
      Charset                  =>
        (Is_Set => True,
         Value  =>
           Ada.Strings.Unbounded.To_Unbounded_String (Default_Charset)));

   Undefined_Basic_Format_Options : constant Basic_Format_Options_Type :=
     (others => <>);

   package String_To_Basic_Format_Options_Hash_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Basic_Format_Options_Type,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   subtype String_To_Basic_Format_Options_Hash_Map is
     String_To_Basic_Format_Options_Hash_Maps.Map;

   type Supported_Languages_Format_Options_Type is
     array (Supported_Languages) of Basic_Format_Options_Type;

   type Format_Options_Type is record
      Language        : Supported_Languages_Format_Options_Type :=
        [others => Undefined_Basic_Format_Options];
      Sources         : String_To_Basic_Format_Options_Hash_Map :=
        String_To_Basic_Format_Options_Hash_Maps.Empty_Map;
      Ignored_Sources : Optional_String_Hashed_Set := (Is_Set => False);
   end record;

   Default_Format_Options : constant Format_Options_Type :=
     (Language        => [others => Default_Basic_Format_Options],
      Sources         => String_To_Basic_Format_Options_Hash_Maps.Empty_Map,
      Ignored_Sources => (Is_Set => False));

   package View_Ids_To_Format_Options_Hashed_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => GPR2.View_Ids.View_Id,
        Element_Type    => Gnatformat.Configuration.Format_Options_Type,
        Hash            => GPR2.View_Ids.Hash,
        Equivalent_Keys => GPR2.View_Ids."=");

   subtype View_Id_To_Format_Options_Hashed_Map is
     View_Ids_To_Format_Options_Hashed_Maps.Map;

   type Project_Format_Options_Cache_Type is tagged record
      Cache : View_Id_To_Format_Options_Hashed_Map;
   end record;

   type Format_Options_Builder_Type is tagged
     record
       Project                             : Optional_GPR2_Project_View;
         Format_Options                    : Format_Options_Type;
         Implicit_Indentation_Continuation : Boolean;
     end record;

   Default_Unparsing_Configuration :
     constant Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
       Langkit_Support.Generic_API.Unparsing.Default_Unparsing_Configuration
         (Language => Libadalang.Generic_API.Ada_Lang_Id);

end Gnatformat.Configuration;
