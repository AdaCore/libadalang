with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   Ctx : constant Analysis_Context := Create_Context;

   function Get_Unit (Filename : String) return Analysis_Unit is
      Unit : constant Analysis_Unit := Get_From_File (Ctx, Filename);
   begin
      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put_Line ("error: " & Filename & ": "
                      & Langkit_Support.Diagnostics.To_Pretty_String (D));
         end loop;
         raise Program_Error with "Parsing error";
      end if;
      return Unit;
   end Get_Unit;

   Unit     : constant Analysis_Unit := Get_Unit ("pkg.ads");
   Decls    : constant Ada_Node_Array :=
      Find (Unit.Root, Kind_Is (Ada_Object_Decl)).Consume;
   D1       : constant Object_Decl := Decls (1).As_Object_Decl;
   D2       : constant Object_Decl := Decls (2).As_Object_Decl;
   N        : constant Expr := D2.F_Default_Expr;
   Resolved : Object_Decl;
begin
   Put_Line ("D1: " & D1.Image);
   Put_Line ("D2: " & D2.Image);

   if not D2.P_Resolve_Names then
      raise Program_Error with "Resolution failed";
   end if;

   Resolved := N.As_Name.P_Referenced_Decl.As_Object_Decl;
   Put_Line ("Resolved: " & Resolved.Image);

   if D1.As_Ada_Node /= D1 then
      raise Program_Error with "Tag makes comparison fail";
   end if;

   if D1.As_Ada_Node = Resolved then
      raise Program_Error with "Entity info ignored";
   end if;

   Put_Line ("Done.");
end Main;
