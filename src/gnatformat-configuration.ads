--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gnatformat.Utils;

with GNATCOLL.VFS;

with GPR2.Project.View;
with GPR2.Project.Attribute;

with Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;

with Prettier_Ada.Documents;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

private with GPR2.View_Ids;

private with Libadalang.Generic_API;

package Gnatformat.Configuration is

   type Format_Options_Type;

   procedure Elaborate_GPR2;
   --  Updates the GPR2 registry with the following:
   --
   --     - package Format
   --     - atribute Width, Indentation, Indentation_Kind and End_Of_Line
   --     indexed by language or source.

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

   type Format_Options_Type is private;

   function From_Project
     (Project : GPR2.Project.View.Object)
      return Format_Options_Type;
   --  Creates a Format_Options_Type by parsing Project's Format package

   function Into
     (Self     : Format_Options_Type;
      Language : Supported_Languages)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Converts a Format_Options_type for the provided Language into an
   --  equivalent Prettier_Ada Format_Options.Type. If there's no configuration
   --  associated to Language, then returns Default_Format_Options.

   function Into
     (Self            : Format_Options_Type;
      Source_Filename : String)
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
     (Target : in out Format_Options_Type;
      Source : Format_Options_Type);
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
     (Self     : in out Project_Format_Options_Cache_Type;
      Project  : GPR2.Project.View.Object)
      return Format_Options_Type;
   --  Retrives the format options associated to Project using a cached result
   --  if existent. Otherwise the format options are computed.

   type Format_Options_Builder_Type is private;

   function Create_Format_Options_Builder return Format_Options_Builder_Type;
   --  Format_Options_Builder_Type constructor.
   --
   --  A builder of Format_Options_Type.

   function Build
     (Self : Format_Options_Builder_Type)
      return Format_Options_Type;
   --  Returns the format options current state

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
   --  TODO

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

   ----------------------------------------------
   --  Getters for Format_Options_Type fields  --
   ----------------------------------------------
   function Get_Width
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Natural;

   function Get_Indentation
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Natural;

   function Get_Indentation_Kind
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Indentation_Kind;

   function Get_Indentation_Continuation
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return Natural;

   function Get_End_Of_Line
     (Options         : Format_Options_Type;
      Source_Filename : String;
      Language        : Supported_Languages := Ada_Language)
      return End_Of_Line_Kind;

   -----------------------------------------------
   --  Helper function for formatting purposes  --
   -----------------------------------------------

   function Load_Unparsing_Configuration
     (Unparsing_Configuration_File : GNATCOLL.VFS.Virtual_File;
      Diagnostics :
        in out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
   --  Loads the formatting rules

private

   Package_Id : constant GPR2.Package_Id := GPR2."+" ("format");

   Width_Attribute_Id   : constant GPR2.Attribute_Id   :=
     GPR2."+" ("width");
   Q_Width_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Width_Attribute_Id);

   Indentation_Attribute_Id   : constant GPR2.Attribute_Id   :=
     GPR2."+" ("indentation");
   Q_Indentation_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Indentation_Attribute_Id);

   Indentation_Kind_Attribute_Id   : constant GPR2.Attribute_Id   :=
     GPR2."+" ("indentation_kind");
   Q_Indentation_Kind_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Indentation_Kind_Attribute_Id);

   Indentation_Continuation_Attribute_Id   : constant GPR2.Attribute_Id   :=
     GPR2."+" ("indentation_continuation");
   Q_Indentation_Continuation_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, Indentation_Continuation_Attribute_Id);

   End_Of_Line_Attribute_Id   : constant GPR2.Attribute_Id   :=
     GPR2."+" ("end_of_line");
   Q_End_Of_Line_Attribute_Id : constant GPR2.Q_Attribute_Id :=
     (Package_Id, End_Of_Line_Attribute_Id);

   type Basic_Format_Options_Type is
     record
       Width                    : Optional_Positive         :=
         (Is_Set => False);
       Indentation              : Optional_Positive         :=
         (Is_Set => False);
       Indentation_Kind         : Optional_Indentation_Kind :=
         (Is_Set => False);
       Indentation_Continuation : Optional_Positive         :=
         (Is_Set => False);
       End_Of_Line              : Optional_End_Of_Line_Kind :=
         (Is_Set => False);
     end record;

   function Indentation_Continuation
     (Self : Basic_Format_Options_Type)
      return Positive;
   --  Returns the continuation line indentation which was explicitly set or
   --  default to Indentation - 1.

   function Into
     (Self : Basic_Format_Options_Type)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Converts a Basic_Format_Options_Type into an equivalent
   --  Prettier_Ada Format_Options.Type.

   procedure Overwrite
     (Target : in out Basic_Format_Options_Type;
      Source : Basic_Format_Options_Type);
   --  Overwrites Target's options by Source's ones

   Default_Basic_Format_Options : constant Basic_Format_Options_Type :=
     (Width                    => (Is_Set => True, Value => 79),
      Indentation              => (Is_Set => True, Value => 3),
      Indentation_Kind         => (Is_Set => True, Value => Spaces),
      Indentation_Continuation => (Is_Set => True, Value => 2),
      End_Of_Line              => (Is_Set => True, Value => LF));

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

   type Format_Options_Type is
     record
       Language : Supported_Languages_Format_Options_Type :=
         [others => Undefined_Basic_Format_Options];
       Sources  : String_To_Basic_Format_Options_Hash_Map :=
         String_To_Basic_Format_Options_Hash_Maps.Empty_Map;
     end record;

   Default_Format_Options : constant Format_Options_Type :=
     (Language => [others => Default_Basic_Format_Options],
      Sources  => String_To_Basic_Format_Options_Hash_Maps.Empty_Map);

   package View_Ids_To_Format_Options_Hashed_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => GPR2.View_Ids.View_Id,
        Element_Type    => Gnatformat.Configuration.Format_Options_Type,
        Hash            => GPR2.View_Ids.Hash,
        Equivalent_Keys => GPR2.View_Ids."=");

   subtype View_Id_To_Format_Options_Hashed_Map is
     View_Ids_To_Format_Options_Hashed_Maps.Map;

   type Project_Format_Options_Cache_Type is tagged
     record
       Cache : View_Id_To_Format_Options_Hashed_Map;
     end record;

   type Format_Options_Builder_Type is tagged
     record
       Format_Options : Format_Options_Type;
     end record;

   Default_Unparsing_Configuration :
     constant Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
       Langkit_Support.Generic_API.Unparsing.Default_Unparsing_Configuration
          (Language => Libadalang.Generic_API.Ada_Lang_Id);

end Gnatformat.Configuration;
