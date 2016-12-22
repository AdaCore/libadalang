with Ada.Command_Line;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;

with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Libadalang.Analysis;   use Libadalang.Analysis;

procedure Symres is

   Ctx   : Analysis_Context := Create;

   Quiet : Boolean := False;

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   --  List of strings. Used to represent the lines in a source file.

   type String_Vector_Access is access String_Vectors.Vector;
   procedure Destroy is new Ada.Unchecked_Deallocation
     (String_Vectors.Vector, String_Vector_Access);

   package Strings_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => String_Vector_Access,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   Lines_Map : Strings_Maps.Map;
   --  Associate a list of source lines for each source visited files

   function "<" (Left, Right : Ada_Node) return Boolean is
     (Left.Sloc_Range.Start_Line < Right.Sloc_Range.Start_Line);
   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Ada_Node,
      Array_Type   => Ada_Node_Array,
      "<"          => "<");

   function Decode_Boolean_Literal (T : Text_Type) return Boolean;
   function Get_Source_Lines (Filename : String) return String_Vector_Access;
   function Source_Slice
     (Lines      : String_Vectors.Vector;
      Sloc_Range : Source_Location_Range)
      return String;
   procedure Put_Title (C : Character; S : String);
   procedure Process_File (Unit : Analysis_Unit; Filename : String);

   --------------
   -- New_Line --
   --------------

   procedure New_Line is begin
      if not Quiet then Ada.Text_IO.New_Line; end if;
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is begin
      if not Quiet then Ada.Text_IO.Put_Line (S); end if;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is begin
      if not Quiet then Ada.Text_IO.Put (S); end if;
   end Put;

   ----------------------------
   -- Decode_Boolean_Literal --
   ----------------------------

   function Decode_Boolean_Literal (T : Text_Type) return Boolean is
   begin
      if T = "True" then
         return True;
      elsif T = "False" then
         return False;
      else
         raise Program_Error with "Invalid boolean value: " & Image (T, True);
      end if;
   end Decode_Boolean_Literal;

   ----------------------
   -- Get_Source_Lines --
   ----------------------

   function Get_Source_Lines (Filename : String) return String_Vector_Access
   is
      Fname : Unbounded_String := To_Unbounded_String (Filename);
      Cur   : Strings_Maps.Cursor;
      use Ada.Text_IO;
      File  : File_Type;
      Lines : String_Vector_Access;
   begin
      --  Look for an already available array of lines for this file
      Cur := Lines_Map.Find (Fname);
      if Strings_Maps.Has_Element (Cur) then
         return Strings_Maps.Element (Cur);
      end if;

      --  There is none, so create one and remember it
      Lines := new String_Vectors.Vector;
      Lines_Map.Insert (Fname, Lines);

      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Lines.Append (To_Unbounded_String (Get_Line (File)));
      end loop;
      Close (File);

      return Lines;
   end Get_Source_Lines;

   ------------------
   -- Source_Slice --
   ------------------

   function Source_Slice
     (Lines      : String_Vectors.Vector;
      Sloc_Range : Source_Location_Range)
      return String
   is
      pragma Assert (Sloc_Range.Start_Line = Sloc_Range.End_Line);
   begin
      return Slice (Lines.Element (Positive (Sloc_Range.Start_Line)),
                    Natural (Sloc_Range.Start_Column),
                    Natural (Sloc_Range.End_Column) - 1);
   end Source_Slice;

   ---------------
   -- Put_Title --
   ---------------

   procedure Put_Title (C : Character; S : String) is
   begin
      Put_Line (S);
      Put_Line ((1 .. S'Length => C));
      New_Line;
   end Put_Title;

   ------------------
   -- Resolve_Node --
   ------------------

   procedure Resolve_Node (N : Ada_Node) is
      function Safe_Image
        (Node : access Ada_Node_Type'Class) return String
      is (if Node = null then "None" else Image (Node.Short_Image));

      function Is_Expr (N : Ada_Node) return Boolean
      is (N.all in Expr_Type'Class);
   begin
      N.Assign_Names_To_Logic_Vars;
      if N.P_Resolve_Symbols then
         for Node of N.Find (Is_Expr'Access).Consume loop
            declare
               P_Ref  : Ada_Node := Expr (Node).P_Ref_Val;
               P_Type : Ada_Node := Expr (Node).P_Type_Val;
            begin
               if not Quiet then
                  Put_Line
                    ("Expr: " & Safe_Image (Node) & ", references "
                     & Safe_Image (P_Ref) & ", type is "
                     & Safe_Image (P_Type));
               end if;
            end;
         end loop;
      else
         Put_Line ("Resolution failed for node " & Safe_Image (N));
      end if;
   end Resolve_Node;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Unit : Analysis_Unit; Filename : String) is

      function Source_Slice (Node : access Ada_Node_Type'Class) return String;

      function Safe_Image
        (Node : access Ada_Node_Type'Class) return String
      is
        (if Node = null then "None" else Image (Node.Short_Image));

      ------------------
      -- Source_Slice --
      ------------------

      function Source_Slice (Node : access Ada_Node_Type'Class) return String
      is
         Unit  : constant Analysis_Unit := Get_Unit (Node);
         Lines : constant String_Vector_Access :=
            Get_Source_Lines (Get_Filename (Unit));
      begin
         return Source_Slice (Lines.all, Node.Sloc_Range);
      end Source_Slice;

   begin
      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put_Line ("error: " & Filename & ":"
                      & Langkit_Support.Diagnostics.To_Pretty_String (D));
         end loop;
         return;
      end if;
      Populate_Lexical_Env (Unit);

      declare
         --  Configuration for this unit
         Display_Slocs : Boolean := False;

         Empty     : Boolean := True;
         Last_Line : Natural := 0;
         It        : Find_Iterator := Find
           (Root (Unit),
            new Ada_Node_Kind_Filter'(Ada_Node_Predicate_Type with
                                      Kind => Ada_Pragma_Node));
         Node        : Ada_Node;

         P_Node      : Pragma_Node;
         Pragma_Name : Text_Access;
      begin
         --  Print what entities are found for expressions X in all the "pragma
         --  Test (X)" we can find in this unit.
         while Next (It, Node) loop

            P_Node := Pragma_Node (Node);
            Pragma_Name := Data (P_Node.F_Id.F_Tok).Text;

            --  If this pragma and the previous ones are not on adjacent lines,
            --  do not make them adjacent in the output.
            if Pragma_Name.all /= "Config" then
               if Last_Line /= 0
                     and then
                  Natural (Node.Sloc_Range.Start_Line) - Last_Line > 1
               then
                  New_Line;
               end if;
               Last_Line := Natural (Node.Sloc_Range.End_Line);
            end if;

            if Pragma_Name.all = "Config" then
               --  Handle testcase configuration pragmas for this file
               for Arg of P_Node.F_Args.Children loop
                  declare
                     A     : constant Pragma_Argument_Assoc :=
                        Pragma_Argument_Assoc (Arg);

                     pragma Assert (A.F_Id.all in Identifier_Type'Class);
                     Name  : constant Text_Type :=
                        Data (A.F_Id.F_Tok).Text.all;

                     pragma Assert (A.F_Expr.all in Identifier_Type'Class);
                     Value : constant Text_Type :=
                        Data (Identifier (A.F_Expr).F_Tok).Text.all;
                  begin
                     if Name = "Display_Slocs" then
                        Display_Slocs := Decode_Boolean_Literal (Value);
                     else
                        raise Program_Error with
                          ("Invalid configuration: " & Image (Name, True));
                     end if;
                  end;
               end loop;

            elsif Pragma_Name.all = "Section" then
               --  Print headlines
               declare
                  pragma Assert (P_Node.F_Args.Child_Count = 1);
                  Arg : constant Expr := P_Node.F_Args.Item (1).F_Expr;
                  pragma Assert (Arg.all in String_Literal_Type'Class);
                  Tok : constant Token_Type := String_Literal (Arg).F_Tok;
                  Text : constant Text_Type := Data (Tok).Text.all;
               begin
                  Put_Title
                    ('-', Image (Text (Text'First + 1 .. Text'Last - 1)));
               end;
               Empty := True;

            elsif Pragma_Name.all = "Test" then
               --  Perform symbol resolution
               declare
                  pragma Assert (P_Node.F_Args.Child_Count = 1);
                  Arg      : constant Expr := P_Node.F_Args.Item (1).F_Expr;
                  Entities : Ada_Node_Array_Access := Arg.P_Entities;
               begin
                  Put_Line (Source_Slice (Arg) & " resolves to:");
                  Sort (Entities.Items);
                  for E of Entities.Items loop
                     Put ("    " & Source_Slice (E));
                     if Display_Slocs then
                        Put_Line (" at " & Image (Start_Sloc (E.Sloc_Range)));
                     else
                        New_Line;
                     end if;
                  end loop;
                  if Entities.N = 0 then
                     Put_Line ("    <none>");
                  end if;
                  Dec_Ref (Entities);
               end;
               Empty := False;

            elsif Pragma_Name.all = "Test_Statement" then
               pragma Assert (P_Node.F_Args.Child_Count = 0);
               Resolve_Node (P_Node.Previous_Sibling);
               Empty := False;

            elsif Pragma_Name.all = "Test_Block" then
               pragma Assert (P_Node.F_Args.Child_Count = 0);
               declare
                  Block : Block_Stmt :=
                    Block_Stmt (P_Node.Previous_Sibling);
                  function Is_Xref_Entry_Point (N : Ada_Node) return Boolean
                  is (N.P_Xref_Entry_Point);
               begin
                  for Node
                    of Block.Find (Is_Xref_Entry_Point'Access).Consume
                  loop
                     Resolve_Node (Node);
                  end loop;
               end;
               Empty := False;
            end if;
         end loop;
         if not Empty then
            New_Line;
         end if;
      end;
   end Process_File;

begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Arg  : constant String := Ada.Command_Line.Argument (I);
         Unit : Analysis_Unit;
      begin
         if Arg in "--quiet" | "-q" then
            Quiet := True;
         else
            Unit := Get_From_File (Ctx, Arg);
            Put_Title ('#', "Analyzing " & Arg);
            Process_File (Unit, Arg);
         end if;
      end;
   end loop;

   for Lines of Lines_Map loop
      Destroy (Lines);
   end loop;

   Destroy (Ctx);
   Put_Line ("Done.");
end Symres;
