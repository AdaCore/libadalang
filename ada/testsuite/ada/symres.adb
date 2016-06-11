with Ada.Command_Line;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with Interfaces; use Interfaces;

with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.AST;        use Libadalang.AST;
with Libadalang.AST.Types;  use Libadalang.AST.Types;

procedure Symres is

   Ctx : Analysis_Context := Create;

   package WT renames Ada.Wide_Wide_Text_IO;

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);

   function "<" (Left, Right : Ada_Node) return Boolean is
     (Left.Sloc_Range.Start_Line < Right.Sloc_Range.Start_Line);
   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Ada_Node,
      Array_Type   => Ada_Node_Array,
      "<"          => "<");

   function Decode_Boolean_Literal (T : Text_Type) return Boolean;
   procedure Get_Source_Lines
     (Filename : String;
      Lines    : out String_Vectors.Vector);
   function Source_Slice
     (Lines      : String_Vectors.Vector;
      Sloc_Range : Source_Location_Range)
      return String;
   procedure Put_Title (C : Character; S : String);
   procedure Process_File (Unit : Analysis_Unit; Filename : String);

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

   procedure Get_Source_Lines
     (Filename : String;
      Lines    : out String_Vectors.Vector)
   is
      File  : File_Type;
   begin
      Lines := String_Vectors.Empty_Vector;
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Lines.Append (To_Unbounded_String (Get_Line (File)));
      end loop;
      Close (File);
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
   -- Process_File --
   ------------------

   procedure Process_File (Unit : Analysis_Unit; Filename : String) is
      Lines : String_Vectors.Vector;

      function Source_Slice (Node : access Ada_Node_Type'Class) return String
      is (Source_Slice (Lines, Node.Sloc_Range));

      function Safe_Image
        (Node : access Ada_Node_Type'Class) return Wide_Wide_String
      is
        (if Node = null then "None" else Node.Short_Image);

   begin
      Get_Source_Lines (Filename, Lines);

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
               for Arg of P_Node.F_Args.all loop
                  declare
                     A     : constant Pragma_Argument := Pragma_Argument (Arg);

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
                  Arg : constant Expr := P_Node.F_Args.Item (0).F_Expr;
                  pragma Assert (Arg.all in String_Literal_Type'Class);
                  Tok : constant Token_Type := String_Literal (Arg).F_Tok;
                  Text : constant Text_Type := Data (Tok).Text.all;
               begin
                  Put_Title
                    ('-', Image (Text (Text'First + 1 .. Text'Last - 1)));
               end;

            elsif Pragma_Name.all = "Test" then
               --  Perform symbol resolution
               declare
                  pragma Assert (P_Node.F_Args.Child_Count = 1);
                  Arg      : constant Expr := P_Node.F_Args.Item (0).F_Expr;
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
            elsif Pragma_Name.all = "Test_Statement" then
               pragma Assert (P_Node.F_Args = null);
               declare
                  St   : Statement := Statement (P_Node.Previous_Sibling);
                  It   : Traverse_Iterator;
                  Node : Ada_Node;
               begin
                  if St.P_Resolve_Symbols then
                     It := St.Traverse;
                     while It.Next (Node) loop
                        case Kind (Node) is
                           when Ada_Expr =>
                              WT.Put_Line
                                ("Expr: " & Safe_Image (Expr (Node))
                                 & ", references "
                                 & Safe_Image (Expr (Node).P_Ref_Val)
                                 & ", type is "
                                 & Safe_Image (Expr (Node).P_Type_Val));
                           when others => null;
                        end case;
                     end loop;
                  else
                     Put_Line ("Resolution failed for statement");
                  end if;
               end;
            end if;
         end loop;
      end;
   end Process_File;

begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Filename : constant String := Ada.Command_Line.Argument (I);
         Unit     : Analysis_Unit := Get_From_File (Ctx, Filename);
      begin
         Put_Title ('#', "Analyzing " & Filename);
         Process_File (Unit, Filename);
         Remove (Ctx, Filename);
      end;
   end loop;

   Destroy (Ctx);
   New_Line;
   Put_Line ("Done.");
end Symres;
