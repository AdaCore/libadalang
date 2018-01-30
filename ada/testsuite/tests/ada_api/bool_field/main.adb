with Ada.Text_IO;          use Ada.Text_IO;

with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   Ctx       : Analysis_Context := Create;
   Unit      : constant Analysis_Unit := Get_From_File (Ctx, "pkg.ads");
   P         : constant Ada_Node_Predicate := new Ada_Node_Kind_Filter'
     (Kind => Ada_Type_Decl);
   Type_Defs : constant Ada_Node_Array :=
      Ada_Node_Array (Find (Root (Unit), P).Consume);
begin
   for T of Type_Defs loop
      declare
         TD   : constant Type_Decl := T.As_Type_Decl;
         RTD  : constant Record_Type_Def := TD.F_Type_Def.As_Record_Type_Def;
         Name : constant Text_Type := TD.F_Name.Text;
      begin
         Put_Line
           (Image (Name) & " is abstract: "
            & Boolean'Image (RTD.F_Has_Abstract));
      end;
   end loop;
   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
