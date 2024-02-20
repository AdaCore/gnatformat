with Ada.Directories;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Ada.Exceptions;           use Ada.Exceptions;

with Gnatformat.Command_Line;
with Gnatformat.Utils;
with Gnatformat.Analysis_Unit_Vectors;

with Prettier_Ada.Documents;
with Prettier_Ada.Documents.Json;

with Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Errors; use Langkit_Support.Errors;

with GNATCOLL.Opt_Parse;

with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Gnatformat.Ada_Driver is
   use type Gnatformat.Command_Line.Sources.Result_Array;
   use Langkit_Support.Generic_API.Unparsing;
   use Prettier_Ada.Documents;
   use GNATCOLL.Opt_Parse;
   use Gnatformat.Command_Line;

   use Ada.Directories;

   procedure Process (Units  : Gnatformat.Analysis_Unit_Vectors.Vector;
                      Config : Unparsing_Configuration);
   --  Procedure used to process each Unit and using the given unparsing
   --  configuration

   procedure Load_Unparsing_Config (Config : out Unparsing_Configuration);
   --  If specified, loads the unparing configuration given as argument of
   --  --config-file. Otherwise uses a built in default configuration.

   -----------------------------
   --  Load_Unparsing_Config  --
   -----------------------------

   procedure Load_Unparsing_Config (Config : out Unparsing_Configuration) is
      --  TO DO : implement this later
      procedure Load_Default_Unparsing_Config
        (Config : out Unparsing_Configuration)
      is null;

   begin
      --  Loading config file
      if Length (Config_Filename.Get) = 0 then
         --  TO DO: get the default configuration for unparsing
         --  (nothing is done yet, to be implemented later)
         Load_Default_Unparsing_Config (Config);

         Gnatformat_Trace.Trace ("ERROR: Incorrect command line! " &
          "Please specify a config file !");

      else
         begin
            Config :=
              Load_Unparsing_Config
                (Gnatformat.Utils.Language,
                 To_String (Gnatformat.Command_Line.Config_Filename.Get));
         exception
            when Exc : Invalid_Input =>
               Put_Line ("Error when loading the unparsing configuration:");
               Put_Line (Exception_Message (Exc));
               return;
         end;
      end if;
   end Load_Unparsing_Config;

   ---------------
   --  Process  --
   ---------------

   procedure Process (Units   : Gnatformat.Analysis_Unit_Vectors.Vector;
                      Config  : Unparsing_Configuration)
   is
      Opt : constant Prettier_Ada.Documents.Format_Options_Type :=
        Prettier_Ada.Documents.Format_Options_Type'
          (Width       => Gnatformat.Command_Line.Width.Get,
           Indentation => (Gnatformat.Command_Line.Indentation_Kind.Get,
                           Gnatformat.Command_Line.Indentation_Width.Get),
           End_Of_Line => Gnatformat.Command_Line.End_Of_Line.Get);
   begin
      for Unit of Units loop
         declare
            F         : File_Type;
            Doc       : constant Document_Type :=
              Unparse_To_Prettier (Unit.Root, Config);
            Formatted : constant Ada.Strings.Unbounded.Unbounded_String :=
              Prettier_Ada.Documents.Format (Document => Doc,
                                             Options  => Opt);
         begin
            --  Dump the intermediate representation JSON file
            if Gnatformat.Command_Line.Dump_Document.Get then
               Create
                 (F, Name => "doc_" & Simple_Name (Unit.Filename) & ".json");
               Put_Line (F, Prettier_Ada.Documents.Json.Serialize (Doc));
               Close (F);
            end if;

            --  Finally, write the formatted source code on the standard output
            --  or in the output folder if requested
            if Length (Gnatformat.Command_Line.Output_Folder.Get) /= 0 then
               --  Create output directory if needed
               declare
                  Dir : constant String :=
                    To_String (Gnatformat.Command_Line.Output_Folder.Get);
                  Cannot_Create : constant String :=
                    "cannot create directory '" & Dir & "'";

                  Out_File_Path : constant String :=
                    Normalize_Pathname
                      (Compose (Dir, Simple_Name (Unit.Filename)));
               begin
                  if Exists (Dir) then
                     if Kind (Dir) /= Directory then
                        Gnatformat_Trace.Trace
                          ("ERROR: Incorrect command line! "
                           & Cannot_Create & "; file already exists");
                     end if;
                  else
                     begin
                        Create_Path (Dir);
                        Set_Directory (Dir);
                     exception
                        when Ada.Directories.Name_Error |
                             Ada.Directories.Use_Error =>
                           Gnatformat_Trace.Trace
                             ("ERROR: Incorrect command line! "
                              & Cannot_Create & "; file already exists");
                     end;
                  end if;
                  Create (F, Mode => Out_File, Name => Out_File_Path);
                  Put_Line (F, Formatted);
                  Close (F);
               end;

            elsif Gnatformat.Command_Line.Pipe.Get then
               Put_Line (Formatted);
            end if;

         end;
      end loop;

   end Process;

   Config   : Unparsing_Configuration;
   --  Stores the unparsing configuration used in the formatting process.

begin
   GNATCOLL.Traces.Parse_Config_File;

   if not Gnatformat.Command_Line.Parser.Parse then
      return;
   end if;

   if Gnatformat.Command_Line.Verbose.Get then
      Gnatformat_Trace.Set_Active (True);
   end if;

   Load_Unparsing_Config (Config);

   --  Handling project or sources command line argument
   --  (it is mandatory to have one of these as argument for the processing)

   if Sources.Get /= Sources.No_Results or else Length (Project.Get) /= 0
   then
      declare
         Src_Files : constant Sources.Result_Array :=
           (if Sources.Get /= Sources.No_Results then Sources.Get
            else Sources.No_Results);

         Units     : constant Gnatformat.Analysis_Unit_Vectors.Vector :=
           (if Src_Files = Sources.No_Results then
               Gnatformat.Utils.Get_Project_Analysis_Units_Vector
              (To_String (Gnatformat.Command_Line.Project.Get))
            else
               Gnatformat.Utils.Get_Analysis_Units_Vector_From_Sources_List
              (Gnatformat.Utils.Sources_List (Src_Files),
               To_String (Gnatformat.Command_Line.Project.Get)));
      begin
         --  Process all units using the unparse API
         Process (Units, Config);
      end;
   else
      --  Error message since either an existing project or a list of source
      --  file is expected.
      Gnatformat_Trace.Trace ("ERROR: Incorrect command line!");
   end if;

end Gnatformat.Ada_Driver;
