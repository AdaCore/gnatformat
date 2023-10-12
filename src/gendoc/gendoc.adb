with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Names;       use Langkit_Support.Names;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Generic_API;      use Libadalang.Generic_API;

procedure Gendoc is

   -------------------------
   -- Command line parser --
   -------------------------

   function To_Grammar_Rule (Arg : String) return Grammar_Rule_Ref;
   --  Look for the grammar rule whose lower-case name is ``Arg`` and return
   --  it. Raise a ``Constraint_Error`` if there is no such rule.

   package Args is
      use GNATCOLL.Opt_Parse;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Parse Ada source code and produce a Prettier Document from"
                 & " it.");

      package Grammar_Rule is new Parse_Option
        (Parser, "-r", "--grammar-rule",
         Arg_Type    => Grammar_Rule_Ref,
         Convert     => To_Grammar_Rule,
         Default_Val => Default_Grammar_Rule (Ada_Lang_Id),
         Help        => "Grammar rule to parse the Ada source code.");

      package Output is new Parse_Option
        (Parser, "-o", "--output",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Write formatted Ada source code to the given file.");

      package Source_File is new Parse_Positional_Arg
        (Parser, "source-file",
         Help     => "Ada source file to parse.",
         Arg_Type => Unbounded_String,
         Convert  => To_Unbounded_String);
   end Args;

   ---------------------
   -- To_Grammar_Rule --
   ---------------------

   function To_Grammar_Rule (Arg : String) return Grammar_Rule_Ref is
      Name : constant Name_Type := Create_Name (To_Text (Arg), Lower);
   begin
      for I in Grammar_Rule_Index'First .. Last_Grammar_Rule (Ada_Lang_Id) loop
         declare
            Rule : constant Grammar_Rule_Ref := From_Index (Ada_Lang_Id, I);
         begin
            if Grammar_Rule_Name (Rule) = Name then
               return Rule;
            end if;
         end;
      end loop;

      return (raise Constraint_Error with "invalid grammar rule: " & Arg);
   end To_Grammar_Rule;

begin
   --  Parse command-line arguments and abort if they are invalid

   if not Args.Parser.Parse then
      return;
   end if;

   --  Parse the given source file using the requested grammar rule

   declare
      Ctx  : constant Lk_Context := Create_Context (Ada_Lang_Id);
      Unit : constant Lk_Unit := Ctx.Get_From_File
        (Filename => To_String (Args.Source_File.Get),
         Rule     => Args.Grammar_Rule.Get);
   begin
      --  TODO??? Translate the parse tree to a Prettier Document and dump it
      --  on the standard output.

      Put_Line (Unit.Root.Image);

      --  If requested, also dump the reformatted Ada source code to the output
      --  file.

      declare
         use Ada.Streams.Stream_IO;

         Filename : constant String := To_String (Args.Output.Get);
         File     : Ada.Streams.Stream_IO.File_Type;
         S        : Stream_Access;
      begin
         if Filename /= "" then
            Create (File, Out_File, Filename);
            S := Stream (File);

            --  TODO??? Reformat Ada source code

            String'Write (S, "formatted source code" & ASCII.LF);
         end if;
      end;
   end;
end Gendoc;
