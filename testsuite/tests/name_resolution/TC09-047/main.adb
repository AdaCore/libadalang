with Ada.Text_IO;         use Ada.Text_IO;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

procedure Main is

   function Pre_Search (Node : Ada_Node'Class) return Visit_Status;

   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit    := Ctx.Get_From_File ("inst.ads");
   CU   : constant Compilation_Unit := Unit.Root.As_Compilation_Unit;
   BD   : constant Basic_Decl       := CU.F_Body.As_Library_Item.F_Item;

   function Pre_Search (Node : Ada_Node'Class) return Visit_Status is
      T_Def : Type_Def;
      T_Dec : Base_Type_Decl;
   begin
      if Node.Kind = Ada_Generic_Formal_Part then
         return Over;
      end if;

      if Node.Kind = Ada_Generic_Package_Instantiation then
         declare
            Gen_Name : constant Libadalang.Analysis.Name :=
              Node.As_Generic_Package_Instantiation.F_Generic_Pkg_Name;
            Gen_Decl : constant Basic_Decl :=
              Gen_Name.P_Relative_Name.As_Name.P_Referenced_Decl;
         begin
            Traverse (Gen_Decl, Pre_Search'Access);
         end;
         return Over;
      end if;

      if Kind (Node) not in Ada_Type_Decl then
         return Into;
      end if;

      if not Node.As_Type_Decl.P_Is_Tagged_Type then
         return Over;
      end if;

      Put_Line ("Found type: " & Node.Image);

      T_Def := F_Type_Def (As_Type_Decl (Node));

      while T_Def.Kind = Ada_Derived_Type_Def loop
         T_Dec := T_Def.As_Derived_Type_Def.F_Subtype_Indication.F_Name.
           P_Relative_Name.As_Name.P_Referenced_Decl.As_Base_Type_Decl.
             P_Canonical_Type;
         T_Def := T_Dec.As_Type_Decl.F_Type_Def;
      end loop;

      Put_Line ("      root: " & T_Dec.Image);

      return Over;
   end Pre_Search;

begin
   Put_Line ("==First traversing==");
      Traverse (BD, Pre_Search'Access);
      New_Line;
      Put_Line ("==Second Traversing==");
      Traverse (BD, Pre_Search'Access);
end Main;
