with Ada.Unchecked_Deallocation;

package body Langkit_Support.AST is

   -----------
   -- Child --
   -----------

   function Child (Node  : AST_Node;
                   Index : Natural) return AST_Node
   is
      Result : AST_Node;
      Exists : Boolean;
   begin
      Get_Child (Node, Index, Exists, Result);
      return (if Exists then Result else null);
   end Child;

   --------------
   -- Traverse --
   --------------

   procedure Traverse (Node : AST_Node;
                       Visit : access function (Node : AST_Node)
                       return Visit_Status)
   is
   begin
      --  TODO??? shouldn't this rather visit Node?

      for I in 1 .. Child_Count (Node) loop
         declare
            Cur_Child  : constant AST_Node := Child (Node, I - 1);
            Status : constant Visit_Status := Visit (Cur_Child);
         begin
            if Status = Into then
               Traverse (Cur_Child, Visit);
            elsif Status = Stop then
               return;
            end if;
         end;
      end loop;
   end Traverse;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range (Node : AST_Node;
                        Snap : Boolean := False) return Source_Location_Range
   is
      Tokens : Token_Vectors.Vector renames Node.Token_Data.Tokens;
      Sloc_Start, Sloc_End : Source_Location;
      use Token_Vectors;
   begin
      if Snap then
         declare
            Tok_Start : constant Natural :=
              Natural'Max (Node.Token_Start - 1, 0);
            Tok_End : constant Natural :=
              Natural'Min (Node.Token_End + 1, Last_Index (Tokens));
         begin
            Sloc_Start := End_Sloc (Get (Tokens, Tok_Start).Sloc_Range);
            Sloc_End :=
              Start_Sloc (Get (Tokens, Tok_End).Sloc_Range);
         end;
      else
         Sloc_Start := Start_Sloc (Get (Tokens, Node.Token_Start).Sloc_Range);
         Sloc_End := End_Sloc (Get (Tokens, Node.Token_End).Sloc_Range);
      end if;
      return Make_Range (Sloc_Start, Sloc_End);
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup (Node : AST_Node;
                    Sloc : Source_Location;
                    Snap : Boolean := False) return AST_Node
   is
      Position : Relative_Position;
      Result   : AST_Node;
   begin
      Lookup_Relative (Node, Sloc, Position, Result, Snap);
      return Result;
   end Lookup;

   -------------
   -- Compare --
   -------------

   function Compare (Node : AST_Node;
                     Sloc : Source_Location;
                     Snap : Boolean := False) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node, Snap), Sloc);
   end Compare;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Node : AST_Node) is
   begin
      if Node /= null then
         Node.Ref_Count := Node.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Node : in out AST_Node) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (AST_Node_Type'Class, AST_Node);
   begin
      if Node /= null then
         Node.Ref_Count := Node.Ref_Count - 1;
         if Node.Ref_Count = 0 then
            Free (Node);
            Deallocate (Node);
         end if;
      end if;
   end Dec_Ref;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension
     (Node : AST_Node;
      ID   : Extension_ID;
      Dtor : Extension_Destructor) return Extension_Access
   is
      use Extension_Vectors;
   begin
      for Slot of Node.Extensions loop
         if Slot.ID = ID then
            return Slot.Extension;
         end if;
      end loop;

      declare
         New_Ext : constant Extension_Access :=
           new Extension_Type'(Extension_Type (System.Null_Address));
      begin
         Append (Node.Extensions,
                 Extension_Slot'(ID        => ID,
                                 Extension => New_Ext,
                                 Dtor      => Dtor));
         return New_Ext;
      end;
   end Get_Extension;

   ----------
   -- Free --
   ----------

   procedure Free (Node : access AST_Node_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Extension_Type, Extension_Access);
      use Extension_Vectors;
      Slot : Extension_Slot;
   begin
      --  Explicit iteration for perf
      for J in 0 .. Last_Index (Node.Extensions) loop
         Slot := Get (Node.Extensions, J);
         Slot.Dtor (AST_Node (Node), Slot.Extension.all);
         Free (Slot.Extension);
      end loop;
   end Free;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative (Node       : AST_Node;
                              Sloc       : Source_Location;
                              Position   : out Relative_Position;
                              Node_Found : out AST_Node;
                              Snap       : Boolean := False) is
      Result : constant Relative_Position :=
        Compare (Node, Sloc, Snap);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Node.Lookup_Children (Sloc, Snap)
                     else null);
   end Lookup_Relative;

end Langkit_Support.AST;
