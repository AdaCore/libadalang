with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx       : constant Analysis_Context := Create_Context;
   U         : constant Analysis_Unit := Ctx.Get_From_File ("test.adb");
   Rec_Iter  : Traverse_Iterator'Class := Find
     (U.Root, Kind_In
       (Ada_Discrete_Base_Subtype_Decl,
        Ada_Synth_Anonymous_Type_Decl));

   Type_Decl : Ada_Node;
begin
   while Rec_Iter.Next (Type_Decl) loop
      begin
         Put_Line ("Shapes for " & Type_Decl.Image & ":");
         declare
            Shapes : constant Shape_Array :=
               Type_Decl.As_Base_Type_Decl.P_Shapes;
         begin
            for Shape of Shapes loop
               declare
                  Comps  : constant Base_Formal_Param_Decl_Array :=
                     Components (Shape);
                  Discrs : constant Discriminant_Values_Array :=
                     Discriminants_Values (Shape);
               begin
                  Put_Line ("Shape {");
                  Put ("  Discriminant Values: ");
                  Put ("[");
                  for D of Discrs loop
                     Put (Image (Text (Discriminant (D))));
                     Put (" => ");
                     Put (Image (Text (Values (D))));
                     Put (", ");
                  end loop;
                  Put_Line ("]");

                  Put ("  Components: ");
                  Put ("[");
                  for C of Comps loop
                     Put (Image (Text (C.P_Defining_Name)) & ", ");
                  end loop;
                  Put_Line ("]");
                  Put_Line ("}");
               end;
            end loop;
         end;
         Put_Line ("");
      exception
         when Property_Error =>
            Put_Line ("PROPERTY_ERROR");
      end;
   end loop;
end Main;
