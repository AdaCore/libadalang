with Langkit_Support.Tokens; use Langkit_Support.Tokens;

package body Langkit_Support.Indent is

   function Find_Start_Column (Line : Unbounded_String) return Natural;

   ------------
   -- Create --
   ------------

   function Create (Root  : AST_Node;
                    Lines : String_Vector_Access) return Indent_Engine
   is
   begin
      return (Root         => Root,
              Lines        => Lines,
              Lines_Indent => <>);
   end Create;

   -------------
   -- Process --
   -------------

   procedure Process (Engine : in out Indent_Engine) is
      Lines : String_Vectors.Vector renames Engine.Lines.all;
   begin
      Compute_Indent_Level (Engine.Root);

      for Cur in Lines.Iterate loop
         declare
            Line : constant Integer := String_Vectors.To_Index (Cur);
            Text : Unbounded_String renames
              String_Vectors.Reference (Lines, Cur);
            Start_Col : constant Natural := Find_Start_Column (Text);

            Indent : Unsigned_16 := 0;
         begin
            if Start_Col /= 0 then
               declare
                  Pos  : constant Source_Location :=
                    (Unsigned_32 (Line),
                     Unsigned_16 (Start_Col + 1));
                  Node : constant AST_Node :=
                    Lookup (Engine.Root, Pos, Snap => True);
               begin
                  Indent := (if Node = null
                             then 0
                             else Node.Indent_Level);
               end;
            end if;
            Engine.Lines_Indent.Append (Indent);
         end;
      end loop;
   end Process;

   ------------
   -- Indent --
   ------------

   function Indent (Engine : Indent_Engine;
                    Line   : Natural) return Unsigned_16 is
   begin
      return Engine.Lines_Indent (Line);
   end Indent;

   -----------------------
   -- Find_Start_Column --
   -----------------------

   function Find_Start_Column (Line : Unbounded_String) return Natural is
   begin
      for I in 1 .. Length (Line) loop
         if Element (Line, I) /= ' ' then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Start_Column;

end Langkit_Support.Indent;
