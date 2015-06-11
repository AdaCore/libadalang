with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Langkit_Support.AST.List is

   use Node_Vectors;

   ----------
   -- Kind --
   ----------

   overriding
   function Kind (Node : access List_Type) return AST_Node_Kind is
      pragma Unreferenced (Node);
   begin
      return List_Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   overriding
   function Kind_Name (Node : access List_Type) return String is
      pragma Unreferenced (Node);
   begin
      return "ASTList";
   end Kind_Name;

   -----------
   -- Image --
   -----------

   overriding
   function Image (Node : access List_Type) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '[');
      for El of Node.Vec loop
         if Length (Result) > 0 then
            Append (Result, ", ");
         end if;
         Append (Result, El.Image);
      end loop;

      Append (Result, ']');
      return To_String (Result);
   end Image;

   -----------------
   -- Child_Count --
   -----------------

   overriding
   function Child_Count (Node : access List_Type)
                         return Natural
   is
   begin
      return Length (Node.Vec);
   end Child_Count;

   ---------------
   -- Get_Child --
   ---------------

   overriding
   procedure Get_Child (Node   : access List_Type;
                        Index  : Natural;
                        Exists : out Boolean;
                        Result : out AST_Node)
   is
   begin
      if Index >= Length (Node.Vec) then
         Exists := False;
      else
         Exists := True;
         Result := AST_Node (Node_Vectors.Get (Node.Vec, Index));
      end if;
   end Get_Child;

   --------------------------
   -- Compute_Indent_Level --
   --------------------------

   overriding
   procedure Compute_Indent_Level (Node : access List_Type) is
   begin
      for Child of Node.Vec loop
         Child.Indent_Level := Node.Indent_Level;
         Child.Compute_Indent_Level;
      end loop;
   end Compute_Indent_Level;

   --------------
   -- Validate --
   --------------

   overriding
   procedure Validate (Node   : access List_Type;
                       Parent : AST_Node := null)
   is
   begin
      if Node.Parent /= Parent then
         raise Program_Error;
      end if;

      for Child of Node.Vec loop
         if Child /= null then
            Child.Validate (AST_Node (Node));
         end if;
      end loop;
   end Validate;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Node  : access List_Type;
                    Level : Natural := 0) is
   begin
      if Length (Node.Vec) = 0 then
         return;
      end if;

      for Child of Node.Vec loop
         if Child /= null then
            Child.Print (Level);
         end if;
      end loop;
   end Print;

   ---------------------
   -- Lookup_Children --
   ---------------------

   overriding
   function Lookup_Children (Node : access List_Type;
                             Sloc : Source_Location;
                             Snap : Boolean := False) return AST_Node
   is
      Child : Node_Access;
   begin
      --  Explicit iteration for perf
      for J in 0 .. Last_Index (Node.Vec) loop
         Child := Get (Node.Vec, J);
         declare
            Position : Relative_Position;
            Result   : AST_Node;
         begin
            Lookup_Relative (Child.all'Access, Sloc, Position, Result, Snap);
            case Position is
               when Before =>
                  return AST_Node (Node);
               when Inside =>
                  return Result;
               when After =>
                  null;
            end case;
         end;
      end loop;
      return AST_Node (Node);
   end Lookup_Children;

   overriding
   procedure Free (Node : access List_Type)
   is
      Child : Node_Access;
   begin
      --  Explicit iteration for perf
      for J in 0 .. Last_Index (Node.Vec) loop
         Child := Get (Node.Vec, J);
         Dec_Ref (AST_Node (Child));
      end loop;
      Destroy (Node.Vec);
   end Free;

end Langkit_Support.AST.List;
