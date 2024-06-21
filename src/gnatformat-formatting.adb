
--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with Libadalang.Generic_API;
with Langkit_Support.Text;
with Libadalang.Common;

with Prettier_Ada.Documents; use Prettier_Ada.Documents;
with Prettier_Ada.Documents.Json;

package body Gnatformat.Formatting is

   ------------
   -- Format --
   ------------

   function Format
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String
   is (Format
         (Libadalang.Generic_API.To_Generic_Unit (Unit),
          Format_Options.Into (Ada_Language),
          Configuration));

   ------------
   -- Format --
   ------------

   function Format
     (Unit           : Langkit_Support.Generic_API.Analysis.Lk_Unit;
      Format_Options : Prettier_Ada.Documents.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Document : constant Prettier_Ada.Documents.Document_Type :=
        Langkit_Support.Generic_API.Unparsing.Unparse_To_Prettier
          (Unit.Root, Configuration);
   begin
      return Prettier_Ada.Documents.Format (Document, Format_Options);
   end Format;

   ------------------------------------------------------------------------

   function Format_Unit
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      --  Unparsing_Config_File : GNATCOLL.VFS.Virtual_File)
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits;
   --  Gnatformat library entry point for the whole Unit formatting

   function Diagnostics_Array_To_Vector
     (Arr : Diagnostics_Array) return Diagnostics_Vectors.Vector;
   --  Convert diagnostics array into dignostics vector

   procedure Find_Matching_Parents
     (Node     : Libadalang.Analysis.Ada_Node'Class;
      Match    : not null access function
        (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;
      Callback : not null access procedure
        (Parent : Libadalang.Analysis.Ada_Node;
         Stop   : in out Boolean));
   --  Iterates through the parents of Node and calls Callback on the
   --  parents where Match returns True. This iterative process stops if
   --  Callback sets Stop to True.

   procedure Get_Selection_Enclosing_Node
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      SL_Range       : Langkit_Support.Slocs.Source_Location_Range;
      Enclosing_Node : out Libadalang.Analysis.Ada_Node);
   --  Retrieves the closest enclosing parent for the given selection range

   function Get_Initial_Indentation
     (Node        : Libadalang.Analysis.Ada_Node;
      Indentation : Natural := 3)
      return Natural;
   --  Returns the initial indentation that needs to be used for the selected
   --  Node formatting

   function Estimate_Indentation
     (Node               : Libadalang.Analysis.Ada_Node;
      Indentation        : Natural := 3;
      Inline_Indentation : Natural := 2)
      return Natural;
   --  Estimate the indentation for Node (assuming that it starts in the
   --  begining of its start line.

   -----------------------------------
   --  Diagnostics_Array_To_Vector  --
   -----------------------------------

   function Diagnostics_Array_To_Vector
     (Arr : Diagnostics_Array) return Diagnostics_Vectors.Vector
   is
      V : Diagnostics_Vectors.Vector := Diagnostics_Vectors.Empty_Vector;
   begin
      for I in Arr'Range loop
         Append
           (Diagnostics => V,
            Sloc_Range  => Arr (I).Sloc_Range,
            Message     => Langkit_Support.Text.To_Text (Arr (I).Message));
      end loop;
      return V;
   end Diagnostics_Array_To_Vector;

   -------------------
   --  Format_Unit  --
   -------------------

   function Format_Unit
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      use Ada.Strings.Unbounded;

      SL_Root        : constant Source_Location_Range := Unit.Root.Sloc_Range;

      Formatted_Str  : constant Unbounded_String :=
        Format (Unit => Unit,
                Format_Options => Options,
                Configuration  => Unparsing_Config);

      Diagnostics_V  : constant Diagnostics_Vectors.Vector :=
        Diagnostics_Array_To_Vector (Unit.Diagnostics);
   begin
      return Formatted_Edits'
        (Unit => Unit,
         Edit =>
           Text_Edit'(Location => SL_Root,
                      Text     => Formatted_Str),
         Formatted   => Unit.Root,
         Diagnostics => Diagnostics_V);
   end Format_Unit;

   ---------------------------
   -- Find_Matching_Parents --
   ---------------------------

   procedure Find_Matching_Parents
     (Node     : Libadalang.Analysis.Ada_Node'Class;
      Match    : not null access function
        (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;
      Callback : not null access procedure
        (Parent : Libadalang.Analysis.Ada_Node;
         Stop   : in out Boolean))
   is
      use Libadalang.Analysis;
      Parent : Ada_Node :=
        (if Node.Is_Null then No_Ada_Node else Node.Parent);
      Stop   : Boolean := False;

   begin
      while not Stop loop
         exit when Parent.Is_Null;

         if Match (Parent) then
            Callback (Parent, Stop);
         end if;

         Parent := Parent.Parent;
      end loop;
   end Find_Matching_Parents;

   ------------------------------------------
   --  Get_Selected_Region_Enclosing_Node  --
   ------------------------------------------

   procedure Get_Selection_Enclosing_Node
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      SL_Range       : Langkit_Support.Slocs.Source_Location_Range;
      Enclosing_Node : out Libadalang.Analysis.Ada_Node)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Langkit_Support.Slocs;

      type Search_Direction is (Forward, Backward);

      function Lookup (Unit  : Analysis_Unit;
                       Token : Libadalang.Common.Token_Reference;
                       Look  : Search_Direction)
                       return Ada_Node;
      --  Finds the next Ada_Node relative to Token. Look param controls the
      --  search direction. If Token already belongs to an Ada_Node, that node
      --  is returned. Returns No_Ada_Node if no node is found or if
      --  Token = No_Token.

      ------------
      -- Lookup --
      ------------

      function Lookup
        (Unit  : Analysis_Unit;
         Token : Libadalang.Common.Token_Reference;
         Look  : Search_Direction)
      return Ada_Node
      is
         Crt_Token      : Token_Reference              := Token;
         Crt_Token_Kind : Libadalang.Common.Token_Kind :=
           Kind (Libadalang.Common.Data (Crt_Token));
      begin
         --  Nothing to do if Aux_Token <=> Token is a No_Token or already
         --  belongs to an Ada_Node.
         while not (Crt_Token = No_Token)
           and then Crt_Token_Kind in Ada_Comment | Ada_Whitespace
         loop
            case Look is
               when Forward =>
                  Crt_Token := Next (Crt_Token);

               when Backward =>
                  Crt_Token := Previous (Crt_Token);
            end case;
            Crt_Token_Kind := Kind (Data (Crt_Token));
         end loop;

         if Crt_Token = No_Token then
            return No_Ada_Node;
         end if;

         return Unit.Root.Lookup
           (Start_Sloc (Sloc_Range (Data (Crt_Token)))).As_Ada_Node;
      end Lookup;

      Crt_Start_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.Start_Line, SL_Range.Start_Column));
      Crt_End_Tok : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'(SL_Range.End_Line, SL_Range.End_Column));

      Crt_Start_Node : constant Ada_Node :=
        Lookup (Unit, Crt_Start_Tok, Forward);
      Crt_End_Node   : constant Ada_Node :=
        Lookup (Unit, Crt_End_Tok, Backward);

      --  This is a variable used to find the first relevant parent of
      --  Crt_Start_Node and Crt_End_Node
      Parent_Node : Ada_Node := No_Ada_Node;

      function Is_Relevant_Parent_Kind
        (Kind : Ada_Node_Kind_Type) return Boolean
      is (Kind in Ada_Decl_Block | Ada_Type_Decl | Ada_Compilation_Unit
                | Ada_Stmt | Ada_Basic_Decl);

      function Is_Relevant_Parent_Node
        (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Is_Relevant_Parent_Kind (Node.Kind));

      procedure Is_Relevant_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node.

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean;
      --  Returns True if one of these nodes is already overlapping the other.

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
      --  Returns the overlapping node.

      function Get_Common_Enclosing_Parent_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node;
      --  Starting from 2 given nodes, get the first enclosing common parent

      --------------------------------------
      -- Is_Relevant_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Relevant_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean)
      is
      begin
         Stop        := True;
         Parent_Node := Parent;
      end Is_Relevant_Parent_Node_Callback;

      -------------------------------
      -- Are_Overlapping_Nodes --
      -------------------------------

      function Are_Overlapping_Nodes
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Boolean
      is
         pragma Assert (Start_Node /= End_Node);
      begin
         return
           (Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then
            Start_Node.Sloc_Range.End_Line < End_Node.Sloc_Range.End_Line)
           or else
             (Start_Node.Sloc_Range.Start_Line < End_Node.Sloc_Range.Start_Line
              and then
              Start_Node.Sloc_Range.End_Line > End_Node.Sloc_Range.End_Line);
      end Are_Overlapping_Nodes;

      --------------------------
      -- Get_Overlapping_Node --
      --------------------------

      function Get_Overlapping_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node
      is
         pragma Assert (Start_Node /= End_Node
                        and then Are_Overlapping_Nodes (Start_Node, End_Node));
      begin
         if Start_Node.Sloc_Range.Start_Line > End_Node.Sloc_Range.Start_Line
            and then
             Start_Node.Sloc_Range.End_Line < End_Node.Sloc_Range.End_Line
         then
            return End_Node;

         elsif
           Start_Node.Sloc_Range.Start_Line < End_Node.Sloc_Range.Start_Line
           and then
             Start_Node.Sloc_Range.End_Line > End_Node.Sloc_Range.End_Line
         then
            return Start_Node;
         end if;

         return No_Ada_Node;
      end Get_Overlapping_Node;

      ----------------------------------------
      --  Get_Common_Enclosing_Parent_Node  --
      ----------------------------------------

      function Get_Common_Enclosing_Parent_Node
        (Start_Node : Ada_Node; End_Node : Ada_Node) return Ada_Node
      is
         pragma Assert
           (Start_Node /= No_Ada_Node and then End_Node /= No_Ada_Node);
      begin
         if Start_Node = End_Node then
            return Start_Node;
         else
            declare
               Start_Parents : constant Ada_Node_Array := Start_Node.Parents;
               End_Parents   : constant Ada_Node_Array := End_Node.Parents;
            begin
               for Idx in Start_Parents'First .. Start_Parents'Last - 1 loop
                  for I of End_Parents loop
                     if Start_Parents (Idx) = I then
                        return I.As_Ada_Node;
                     end if;
                  end loop;
               end loop;
            end;
         end if;

         return No_Ada_Node;
      end Get_Common_Enclosing_Parent_Node;

      Start_Node, End_Node : Ada_Node := No_Ada_Node;

      --  Start of Get_Selection_Enclosing_Node
   begin
      Enclosing_Node := No_Ada_Node;
      Parent_Node    := Crt_Start_Node;

      --  Find the first relevant parent of Crt_Start_Node
      if not Is_Relevant_Parent_Kind (Kind (Crt_Start_Node)) then
         Find_Matching_Parents
           (Crt_Start_Node,
            Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      Start_Node := Parent_Node.As_Ada_Node;

      --  Find the first relevant parent of Crt_End_Node
      Parent_Node := Crt_End_Node.As_Ada_Node;

      if not Is_Relevant_Parent_Kind (Kind (Crt_End_Node)) then
         Find_Matching_Parents
           (Crt_End_Node,
            Is_Relevant_Parent_Node'Access,
            Is_Relevant_Parent_Node_Callback'Access);
      end if;
      End_Node := Parent_Node.As_Ada_Node;

      --  When the selection contains different parts of different nodes,
      --  find the first encolsing parent node, otherwise the Enclosing_Node
      --  will be equal to Start_Node or End_Node in some situations.
      if Start_Node /= End_Node then
         if Are_Overlapping_Nodes (Start_Node, End_Node) then
            Enclosing_Node := Get_Overlapping_Node (Start_Node, End_Node);
         else
            Enclosing_Node :=
              Get_Common_Enclosing_Parent_Node (Start_Node, End_Node);

            Parent_Node := Enclosing_Node;
            if Enclosing_Node /= No_Ada_Node
              and then not Is_Relevant_Parent_Kind (Kind (Enclosing_Node))
            then
               Find_Matching_Parents
                 (Enclosing_Node,
                  Is_Relevant_Parent_Node'Access,
                  Is_Relevant_Parent_Node_Callback'Access);
               Enclosing_Node := Parent_Node.As_Ada_Node;
            end if;
         end if;
         pragma Assert (Enclosing_Node /= No_Ada_Node);

      else
         Enclosing_Node := Start_Node;
      end if;

   end Get_Selection_Enclosing_Node;

   -------------------------------
   --  Get_Initial_Indentation  --
   -------------------------------

   function Get_Initial_Indentation
     (Node        : Libadalang.Analysis.Ada_Node;
      Indentation : Natural := 3)
     return Natural
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Langkit_Support.Slocs;

      Parent_Node : Ada_Node := No_Ada_Node;

      function Get_Parent_Indentation (Node : Ada_Node) return Natural;
      --  Returns the Node's parent indentation

      function Is_Expected_Parent_Kind
        (Kind : Ada_Node_Kind_Type) return Boolean
      is (Kind in Ada_Package_Body | Ada_Package_Decl |
                  Ada_Library_Item | Ada_Subp_Body | Ada_Task_Body |
                  Ada_Decl_Block | Ada_For_Loop_Stmt | Ada_Loop_Stmt |
                  Ada_While_Loop_Stmt | Ada_If_Stmt_Range |
                  Ada_Case_Stmt_Range | Ada_Case_Stmt_Alternative_Range);

      function Is_Expected_Parent_Node
        (Node : Ada_Node'Class) return Boolean
      is (not Node.Is_Null and then Is_Expected_Parent_Kind (Node.Kind));

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean);
      --  When Parent is a relevant node stop the search and set Parent_Node

      --------------------------------------
      -- Is_Expected_Parent_Node_Callback --
      --------------------------------------

      procedure Is_Expected_Parent_Node_Callback
        (Parent : Ada_Node; Stop : in out Boolean)
      is
      begin
         Stop        := True;
         Parent_Node := Parent;
      end Is_Expected_Parent_Node_Callback;

      ---------------------------
      -- Get_Parent_Indenation --
      ---------------------------

      function Get_Parent_Indentation (Node : Ada_Node) return Natural
      is
         Offset : Natural := 0;
      begin
         Parent_Node := Node;
         Find_Matching_Parents (Node,
                                Is_Expected_Parent_Node'Access,
                                Is_Expected_Parent_Node_Callback'Access);

         if Kind (Parent_Node) = Ada_Library_Item
           and then Natural (Parent_Node.Sloc_Range.Start_Line) = 1
           and then Natural (Parent_Node.Sloc_Range.Start_Column) > 0
         then
            Offset := 0;
         else
            Offset := Natural (Parent_Node.Sloc_Range.Start_Column) - 1;
         end if;

         case Kind (Parent_Node) is
            when Ada_Package_Body | Ada_Package_Decl
               | Ada_Task_Body | Ada_Subp_Body | Ada_Decl_Block
               | Ada_For_Loop_Stmt | Ada_Loop_Stmt | Ada_While_Loop_Stmt
               | Ada_If_Stmt_Range | Ada_Case_Stmt_Range
               | Ada_Case_Stmt_Alternative_Range
               => Offset := Offset + Indentation;

            when others => null;
         end case;

         return Offset;
      end Get_Parent_Indentation;

      ----------------------------
      --  Get_Previous_Sibling  --
      ----------------------------

      function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node
      is (if Node /= No_Ada_Node then Node.Previous_Sibling.As_Ada_Node
          else No_Ada_Node);
      --  Returns the Node previous sibling or No_Ada_Node if no sibling found

      ------------------------
      --  Get_Next_Sibling  --
      ------------------------

      function Get_Next_Sibling (Node : Ada_Node) return Ada_Node
      is (if Node /= No_Ada_Node then Node.Next_Sibling.As_Ada_Node
          else No_Ada_Node);
      --  Returns the Node next sibling or No_Ada_Node if no sibling found

      Prev_Sibling : constant Ada_Node := Get_Previous_Sibling (Node);
      Next_Sibling : constant Ada_Node := Get_Next_Sibling (Node);
      Offset       : Natural := 0;

   begin
      if Node.Kind in Ada_Ada_List then
         Offset :=
           (if Node.Sloc_Range.Start_Column = 0 then 0
            else Natural (Node.Sloc_Range.Start_Column) - 1);

      elsif Node.Kind in Ada_Subp_Spec_Range then
         --  Subp_Spec nodes can have an overriding node sibling. The correct
         --  offset is given by the enclosing declaration, which is the
         --  parent node.
         Offset :=
           Get_Initial_Indentation
             (Node.P_Parent_Basic_Decl.As_Ada_Node, Indentation);

      elsif (not Prev_Sibling.Is_Null and not Next_Sibling.Is_Null)
        and then Prev_Sibling.Sloc_Range.Start_Column =
                   Next_Sibling.Sloc_Range.Start_Column
      then
         Offset := (if Prev_Sibling.Sloc_Range.Start_Column = 0 then 0
                    else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);

      elsif not Prev_Sibling.Is_Null then
         if Node.Kind in Ada_Subp_Body | Ada_Package_Body | Ada_Package_Decl
                       | Ada_Generic_Package_Renaming_Decl
         then
            if Prev_Sibling.Kind = Ada_Private_Absent
              and then Next_Sibling.Is_Null
            then
               --  Get the parent node which should be a Library_Item which
               --  will give us the offset to use for the reformatting
               Offset := Get_Parent_Indentation (Node);
            else
               Offset :=
                 (if Prev_Sibling.Sloc_Range.Start_Column = 0 then 0
                  else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);
            end if;
         else
            Offset :=
              (if Prev_Sibling.Sloc_Range.Start_Column = 0 then 0
               else Natural (Prev_Sibling.Sloc_Range.Start_Column) - 1);
         end if;

      elsif not Next_Sibling.Is_Null then
         Offset := (if Next_Sibling.Sloc_Range.Start_Column = 0 then 0
                    else Natural (Next_Sibling.Sloc_Range.Start_Column) - 1);

      elsif Prev_Sibling.Is_Null and Next_Sibling.Is_Null then
         --  We should look backward for the Node parent to find the offset
         --  of the parent and compute the one related to the reformatted node
         --  based on gnatpp indentation and indent continuation parameters
         Offset := Get_Parent_Indentation (Node);

      else
         Offset := (if Node.Sloc_Range.Start_Column = 0 then 0
                    else Natural (Node.Sloc_Range.Start_Column) - 1);
      end if;

      return Offset;
   end Get_Initial_Indentation;

   --------------------------
   -- Estimate_Indentation --
   --------------------------

   function Estimate_Indentation
     (Node               : Libadalang.Analysis.Ada_Node;
      Indentation        : Natural := 3;
      Inline_Indentation : Natural := 2)
      return Natural
   is
      use Libadalang.Analysis;

      function Parent_Based_Indentation
        (Parents            : Ada_Node_Array;
         Indentation        : Positive := 3;
         Inline_Indentation : Positive := 2)
      return Natural;
      --  Computes Indentation starting at zero and incrementing based on the
      --  Parents kind or returning earlier if finds a parent that always sets
      --  indentation, for instance, a parameter list.

      ------------------------------
      -- Parent_Based_Indentation --
      ------------------------------

      function Parent_Based_Indentation
        (Parents            : Ada_Node_Array;
         Indentation        : Positive := 3;
         Inline_Indentation : Positive := 2)
      return Natural
      is
         use Libadalang.Common;

         Current_Indentation : Natural := 0;
      begin
         for Parent of Parents loop
            case Parent.Kind is
            when Ada_Loop_Stmt_Range
               | Ada_For_Loop_Stmt_Range
               | Ada_While_Loop_Stmt_Range
               | Ada_If_Stmt_Range
               | Ada_Case_Stmt_Range
               | Ada_Case_Stmt_Alternative_Range
               | Ada_Record_Type_Def_Range
               | Ada_Generic_Formal_Part_Range
               | Ada_Begin_Block_Range
               | Ada_Decl_Block_Range =>
               Current_Indentation := @ + Indentation;

            when Ada_Declarative_Part_Range =>
               --  When we type declare, a DeclBlock is created but not a
               --  DeclarativePart one. Only when you close the block with an
               --  end the node is created.
               --  DeclarativePart is a node that adds indentation.
               --  We cannot simply make DeclBlock also add indentation because
               --  it would double indent. So only add indentation to
               --  DeclarativeParts if their parent is not  DeclBlock.
               if Parent.Parent.Kind not in Ada_Decl_Block_Range then
                  Current_Indentation := @ + Indentation;
               end if;

            when Ada_Handled_Stmts_Range =>
               --  HandledStmts can be children of DeclBlock and BeginBlock.
               --  These two add indentation, so HandledStmts should not
               --  double add if its their child.
               if Parent.Parent.Kind not in
                 Ada_Begin_Block_Range | Ada_Decl_Block_Range
               then
                  Current_Indentation := @ + Indentation;
               end if;

            when Ada_Subp_Spec_Range | Ada_Assign_Stmt_Range =>
               Current_Indentation := @ + Inline_Indentation;

            when Ada_Dotted_Name_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1
                 + Inline_Indentation;
               exit;

               when Ada_Params_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1 + 1;
               exit;

               when Ada_Assoc_List_Range | Ada_Component_List_Range =>
               Current_Indentation :=
                 Natural (Parent.Sloc_Range.Start_Column) - 1;
               exit;

               when others =>
               null;
            end case;
         end loop;

         return Current_Indentation;
      end Parent_Based_Indentation;

      Parents    : constant Ada_Node_Array :=
        (if Node.Is_Null then [] else Node.Parents (False));

   begin
      return
        Parent_Based_Indentation (Parents, Indentation, Inline_Indentation);
   end Estimate_Indentation;

   --------------------
   --  Range_Format  --
   --------------------

   function Range_Format
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Input_Selection_Range : Langkit_Support.Slocs.Source_Location_Range;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      --  use Ada.Strings.Unbounded;

      Enclosing_Node        : Ada_Node := No_Ada_Node;
      Initial_Indentation   : Natural  := 0;
      Indentation_Offset    : Indentation_Offset_Type := (0, 0);
      Estimated_Indentation : Natural := 0;

      Format_Options        : Prettier_Ada.Documents.Format_Options_Type :=
        Options.Into (Ada_Language);
      Offset_Set       : Boolean := False;
   begin
      if Input_Selection_Range = No_Source_Location_Range then
      --  If no selection range is provided dispatch to format the whole unit
         return Format_Unit (Unit             => Unit,
                             Options          => Options,
                             Unparsing_Config => Unparsing_Config);
      end if;

      --  If an input selection is provided then follow the steps to get a
      --  partial formatting of the file for the encolsing node of the given
      --  selection

      --  1. Find the corresponding enclosing node given the initial
      --     selection range.

      Get_Selection_Enclosing_Node
        (Unit           => Unit,
         SL_Range       => Input_Selection_Range,
         Enclosing_Node => Enclosing_Node);
      pragma Assert (Enclosing_Node /= No_Ada_Node);

      --  2. Compute the offset for the indentation of the enclosing node
      --     based on the previous or next sibling starting column position and
      --     also the estimated indentation. If these are different use the
      --     estimated value instead of the initial indetation since if the
      --     indentations of siblings are wrong we get a wrong value for it.
      --     and set this value for further usage by prettier.

      declare
         use Ada.Directories;
         Current_Indentation         : constant Natural :=
           Gnatformat.Configuration.Get_Indentation
             (Options         => Options,
              Source_Filename => Simple_Name (Unit.Get_Filename),
              Language        => Ada_Language);

         Current_Continuation_Indent : constant Natural :=
           Gnatformat.Configuration.Get_Continuation_Line
             (Options         => Options,
              Source_Filename => Simple_Name (Unit.Get_Filename),
              Language        => Ada_Language);
      begin
         Initial_Indentation := Get_Initial_Indentation
           (Node        => Enclosing_Node,
            Indentation => Current_Indentation);

         Estimated_Indentation := Estimate_Indentation
           (Node               => Enclosing_Node,
            Indentation        => Current_Indentation,
            Inline_Indentation => Current_Continuation_Indent);

         if Initial_Indentation /= Estimated_Indentation then
            Initial_Indentation := Estimated_Indentation;
         end if;

         --  Update the Indentation_Offset value of Format_Opt using the
         --  computed offset; this updated value is passed to prettier as
         --  format options when calling Prettier_Ada.Documents.Format at the
         --  next step with the document resulting from the unparsing of the
         --  Enclosing_Node.

         if Format_Options.Indentation.Kind = Spaces then
            Indentation_Offset := (Tabs => 0, Spaces => Initial_Indentation);
         else
            --  Convert the offset into tabs usage compatible value
            Indentation_Offset :=
              (Tabs   =>
                 Initial_Indentation / Format_Options.Indentation.Width,
               Spaces =>
                 Initial_Indentation mod Format_Options.Indentation.Width);
         end if;

         if Format_Options.Indentation.Offset /= Indentation_Offset then
            Format_Options.Indentation.Offset := Indentation_Offset;
            Offset_Set := True;
         end if;
      end;

      --  3. Rewrite the enclosing node relative to the input selection and
      --     return the formatted edits with the new SLOC related to the text
      --     edit to be used by the IDE's for the range formatting.
      declare
         use Langkit_Support.Generic_API;

         Document : constant Prettier_Ada.Documents.Document_Type :=
           Unparsing.Unparse_To_Prettier
             (Libadalang.Generic_API.To_Generic_Node (Enclosing_Node),
              Unparsing_Config);

         Diagnostics     : constant Diagnostics_Vectors.Vector :=
           Diagnostics_Array_To_Vector (Unit.Diagnostics);
         Text_Edit_Sloc  : Langkit_Support.Slocs.Source_Location_Range :=
           Enclosing_Node.Sloc_Range;
      begin
         if Offset_Set and then Enclosing_Node.Sloc_Range.Start_Column /= 1
         then
            --  Reset the formatted node start column position to be able to
            --  get the right indented text edit (to trim the spaces added as
            --  offset) when the formatted node is rewritten
            Text_Edit_Sloc.Start_Column := 1;
         end if;

         --  Create the doc.json file to dump the generated document
         declare
            use Ada.Text_IO;
            F  : File_Type;
         begin
            Create (F, Name => "doc.json");
            Ada.Text_IO.Unbounded_IO.Put_Line
              (F, Prettier_Ada.Documents.Json.Serialize (Document));
            Close (F);
         end;

         return Formatted_Edits'
           (Unit => Unit,
            Edit =>
              Text_Edit'
                (Location => Text_Edit_Sloc,
                 Text     =>
                   Prettier_Ada.Documents.Format (Document, Format_Options)),
            Formatted   => Enclosing_Node,
            Diagnostics => Diagnostics);
      end;
   end Range_Format;

   -----------
   -- Image --
   -----------

   function Image (Edit : Formatted_Edits) return String is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Langkit_Support.Slocs;
   begin
      return
        Simple_Name (Edit.Unit.Get_Filename)
        & "("
        & Edit.Formatted.Image
        & ") - "
        & Image (Edit.Edit.Location)
        & Ada.Characters.Latin_1.LF
        & '^'
        & Ada.Characters.Latin_1.LF
        & To_String (Edit.Edit.Text)
        & '$';
   end Image;

end Gnatformat.Formatting;
