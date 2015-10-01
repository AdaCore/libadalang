with Langkit_Support.Bump_Ptr.Vectors;

generic
   type Node_Type is abstract new AST_Node_Type with private;
   type Node_Access is access all Node_Type'Class;
package Langkit_Support.AST.List is

   List_Kind : constant AST_Node_Kind := 1;

   package Node_Vectors is new Langkit_Support.Bump_Ptr.Vectors
     (Element_Type => Node_Access);

   type List_Type is new AST_Node_Type with record
      Vec : Node_Vectors.Vector;
   end record;

   overriding
   function Kind (Node : access List_Type) return AST_Node_Kind;
   overriding
   function Kind_Name (Node : access List_Type) return String;
   overriding
   function Image (Node : access List_Type) return String;

   overriding
   function Child_Count (Node : access List_Type)
                         return Natural;
   overriding
   procedure Get_Child (Node   : access List_Type;
                        Index  : Natural;
                        Exists : out Boolean;
                        Result : out AST_Node);

   overriding
   procedure Validate (Node : access List_Type;
                       Parent : AST_Node := null);

   overriding
   procedure Print (Node  : access List_Type;
                    Level : Natural := 0);

   overriding
   function Lookup_Children (Node : access List_Type;
                             Sloc : Source_Location;
                             Snap : Boolean := False) return AST_Node;

   overriding procedure Destroy (Node : access List_Type);

end Langkit_Support.AST.List;
