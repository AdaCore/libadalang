with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.AST;       use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;

procedure Main is
   Ctx    : Analysis_Context := Create;
   Unit   : Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   CU     : constant Compilation_Unit := Compilation_Unit (Root (Unit));
   LI     : constant Basic_Decl := F_Item (Library_Item (F_Body (CU)));
   S_Spec : constant Subprogram_Spec := F_Subp_Spec (Subprogram_Body (LI));

   S_Name : constant Libadalang.AST.Types.Name := F_Name (S_Spec);
   Params : constant List_Param_Spec := F_Params (S_Spec);

   ---------------
   -- Put_Param --
   ---------------

   procedure Put_Param (N : Ada_Node) is
      P  : constant Param_Spec := Param_Spec (N);
      Id : Single_Tok_Node;
   begin
      pragma Assert (F_Ids (P).Child_Count = 1);
      Id := Single_Tok_Node (F_Ids (P).Child (1));
      Put (' ');
      Put (Text (F_Tok (Id)));
   end Put_Param;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (N : Ada_Node) is
      pragma Unreferenced (N);
   begin
      Put_Line ("ERROR: not supposed to be called!");
   end Put_Error;

begin
   Put_Line ("Got" & Natural'Image (Params.Child_Count) & " params:");
   Put (" ");
   for N of Params.all loop
      Put_Param (N);
   end loop;
   New_Line;

    Put_Line ("Or if you prefer, in reverse order:");
    Put (" ");
    for N of reverse Params.all loop
       Put_Param (N);
    end loop;
    New_Line;

   Put_Line ("This should be an empty list of local declarations:");
   for N of S_Name.all loop
      Put_Error (N);
   end loop;

   Put_Line ("Or if you prefer, in reverse order:");
   for N of reverse S_Name.all loop
      Put_Error (N);
   end loop;

   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
