package body Xrefs_Wrapper is

   function Subp_Spec_Params (Spec : Subp_Spec) return Param_Spec_List is
     (Spec.F_Subp_Params.F_Params);
   function Subp_Decl_Params (Decl : Basic_Subp_Decl) return Param_Spec_List is
     (Subp_Spec_Params (Decl.P_Subp_Decl_Spec));

   -------------------------
   -- Record_Discriminant --
   -------------------------

   function Record_Discriminant (Node : Ada_Node'Class) return Basic_Decl is
   begin
      if Node.Kind /= Ada_Identifier
        or else Node.Parent.Parent.Kind /= Ada_Discriminant_Spec
      then
         return No_Basic_Decl;
      end if;

      return Node.Parent.Parent.P_Semantic_Parent.As_Basic_Decl;
   end Record_Discriminant;

   ----------------------
   -- Subp_Body_Formal --
   ----------------------

   function Subp_Body_Formal (Decl : Basic_Decl'Class) return Basic_Decl is
      Subp_Body : Ada_Node;
      Subp_Decl : Basic_Subp_Decl;
   begin
      if Decl.Kind /= Ada_Param_Spec then
         return No_Basic_Decl;
      end if;

      Subp_Body := Decl.P_Semantic_Parent;

      --  TODO: remove the .Is_Null check. For now, P_Semantic_Parent can
      --  return a null node in the context of a generic with rebindings.
      if Subp_Body.Is_Null or else Subp_Body.Kind /= Ada_Subp_Body then
         return No_Basic_Decl;
      end if;

      declare
         Decl_Part : constant Basic_Decl := Subp_Body.As_Subp_Body.P_Decl_Part;
      begin
         if Decl_Part.Is_Null then
            return No_Basic_Decl;
         elsif Decl_Part.Kind = Ada_Generic_Subp_Decl then
            Subp_Decl :=
              Decl_Part.As_Generic_Subp_Decl.F_Subp_Decl.As_Basic_Subp_Decl;
         else
            Subp_Decl := Decl_Part.As_Basic_Subp_Decl;
         end if;
      end;

      declare
         Decl_Params : constant Param_Spec_List :=
           Subp_Decl_Params (Subp_Decl);
         Formal_Index : constant Positive := Decl.Child_Index + 1;
      begin
         return Decl_Params.Child (Formal_Index).As_Basic_Decl;
      end;
   end Subp_Body_Formal;

   ---------------
   -- Subp_Body --
   ---------------

   function Subp_Body (Decl : Basic_Decl'Class) return Basic_Decl is
   begin
      if Decl.Kind /= Ada_Subp_Body then
         return No_Basic_Decl;
      end if;

      return Decl.As_Subp_Body.P_Decl_Part;
   end Subp_Body;

   ---------------------
   -- Generic_Package --
   ---------------------

   function Generic_Package (Decl : Basic_Decl'Class) return Basic_Decl is
   begin
      if Decl.Kind /= Ada_Generic_Package_Decl then
         return No_Basic_Decl;
      end if;

      return Decl.As_Generic_Package_Decl.F_Package_Decl.As_Basic_Decl;
   end Generic_Package;

   ------------------
   -- Generic_Subp --
   ------------------

   function Generic_Subp (Decl : Basic_Decl'Class) return Basic_Decl is
   begin
      if Decl.Kind /= Ada_Generic_Subp_Decl then
         return No_Basic_Decl;
      end if;

      return Decl.As_Generic_Subp_Decl.F_Subp_Decl.As_Basic_Decl;
   end Generic_Subp;

   ------------------
   -- Private_Type --
   ------------------

   function Private_Type (Decl : Basic_Decl'Class) return Basic_Decl is
   begin
      if Decl.Kind not in Ada_Base_Type_Decl then
         return No_Basic_Decl;
      end if;

      return
        (Decl.As_Base_Type_Decl
         .P_Previous_Part (Go_To_Incomplete => True)
         .As_Basic_Decl);
   end Private_Type;

end Xrefs_Wrapper;
