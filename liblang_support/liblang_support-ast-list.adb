with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Liblang_Support.AST.List is

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
      for Child of Node.Vec loop
         if Length (Result) > 0 then
            Append (Result, ", ");
         end if;
         Append (Result, Child.Image);
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
      return Natural (Node.Vec.Length);
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
      if Index >= Natural (Node.Vec.Length) then
         Exists := False;
      else
         Exists := True;
         Result := AST_Node (Node.Vec.Element (Index));
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
   procedure Validate (Node : access List_Type) is
   begin
      for Child of Node.Vec loop
         if Child /= null then
            if Child.Parent = null then
               raise Program_Error;
            end if;
            Child.Validate;
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
      if Node.Vec.Is_Empty then
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
                             Snap : Boolean := False) return AST_Node is
   begin
      for Child of Node.Vec loop
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
   procedure Free (Node : access List_Type) is
   begin
      for Child of Node.Vec loop
         Dec_Ref (AST_Node (Child));
      end loop;
   end Free;

end Liblang_Support.AST.List;
