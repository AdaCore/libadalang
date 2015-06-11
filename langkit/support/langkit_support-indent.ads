with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with Langkit_Support.AST; use Langkit_Support.AST;

package Langkit_Support.Indent is

   type Indent_Engine is private;

   package String_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   type String_Vector_Access is access all String_Vectors.Vector;

   function Create (Root  : AST_Node;
                    Lines : String_Vector_Access) return Indent_Engine;

   procedure Process (Engine : in out Indent_Engine);

   function Indent (Engine : Indent_Engine;
                    Line   : Natural) return Unsigned_16;

private

   package Column_Vectors is new Ada.Containers.Vectors
     (Positive, Unsigned_16);

   type Indent_Engine is record
      Root         : AST_Node;
      Lines        : String_Vector_Access;
      Lines_Indent : Column_Vectors.Vector;
   end record;

end Langkit_Support.Indent;
