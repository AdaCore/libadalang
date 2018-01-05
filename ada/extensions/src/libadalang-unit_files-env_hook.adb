package body Libadalang.Unit_Files.Env_Hook is

   procedure Handle_Unit_With_Parents
     (Ctx : Analysis_Context; Node : Bare_Basic_Decl);
   --  Helper for the environment hook to handle library-level unit decl nodes

   procedure Handle_Unit_Body (Ctx : Analysis_Context; Node : Bare_Body);
   --  Helper for the environment hook to handle library-level unit body nodes

   procedure Handle_Subunit (Ctx : Analysis_Context; Node : Bare_Basic_Decl);
   --  Helper for the environment hook to handle sub-units (separates)

   --------------
   -- Env_Hook --
   --------------

   procedure Env_Hook (Unit : Analysis_Unit; Node : Bare_Ada_Node) is
      Ctx : constant Analysis_Context := Get_Context (Unit);
   begin
      if Node.Parent.all in Bare_Library_Item_Type'Class then
         if Node.all in Bare_Body_Type'Class then
            Handle_Unit_Body (Ctx, Bare_Body (Node));
         elsif Node.all in Bare_Basic_Decl_Type'Class then
            Handle_Unit_With_Parents (Ctx, Bare_Basic_Decl (Node));
         end if;
      elsif Node.Parent.all in Bare_Subunit_Type'Class then
         Handle_Subunit (Ctx, Bare_Basic_Decl (Node));
      end if;
   end Env_Hook;

   ------------------------------
   -- Handle_Unit_With_Parents --
   ------------------------------

   procedure Handle_Unit_With_Parents
     (Ctx : Analysis_Context; Node : Bare_Basic_Decl)
   is
      N : Bare_Name;
   begin
      --  If this not a library-level subprogram/package decl, there is no
      --  parent spec to process.
      if Node.all not in
         Bare_Package_Decl_Type'Class
         | Bare_Basic_Subp_Decl_Type'Class
         | Bare_Package_Renaming_Decl_Type'Class
         | Bare_Generic_Package_Decl_Type'Class
         | Bare_Generic_Package_Instantiation_Type'Class
         | Bare_Generic_Subp_Instantiation_Type'Class
         | Bare_Generic_Subp_Decl_Type'Class
         | Bare_Subp_Body_Type'Class
      then
         return;
      end if;

      N := Node.P_Defining_Name.El;

      if N.all in Bare_Dotted_Name_Type'Class then
         declare
            Dummy : constant Analysis_Unit := Fetch_Unit
              (Ctx,
               Bare_Ada_Node (Bare_Dotted_Name (N).F_Prefix),
               Unit_Specification,
               Load_If_Needed => True);
         begin
            null;
         end;
      end if;
   end Handle_Unit_With_Parents;

   --------------------
   -- Handle_Subunit --
   --------------------

   procedure Handle_Subunit (Ctx : Analysis_Context; Node : Bare_Basic_Decl)
   is
      --  Sub-unit handling is very simple: We just want to fetch the
      --  containing unit.
      Dummy : constant Analysis_Unit := Fetch_Unit
        (Ctx, Bare_Ada_Node (Bare_Subunit (Node.Parent).F_Name), Unit_Body,
         Load_If_Needed => True);
   begin
      null;
   end Handle_Subunit;

   ----------------------
   -- Handle_Unit_Body --
   ----------------------

   procedure Handle_Unit_Body (Ctx : Analysis_Context; Node : Bare_Body) is
      Names : Entity_Name_Array_Access;
   begin
      --  If this not a library-level subprogram/package body, there is no spec
      --  to process.
      if Node.all not in Bare_Package_Body_Type'Class
         and then Node.all not in Bare_Subp_Body_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;
      pragma Assert (Names.N = 1);

      declare
         N     : constant Bare_Ada_Node := Bare_Ada_Node (Names.Items (1).El);
         Dummy : Analysis_Unit;
      begin
         Dec_Ref (Names);
         Dummy := Fetch_Unit (Ctx, N, Unit_Specification,
                              Load_If_Needed => True);
      end;

      if Node.all in Bare_Subp_Body_Type'Class then
         --  A library level subprogram body does not have to have a spec. So
         --  we have to compute the parents directly from here.
         Handle_Unit_With_Parents (Ctx, Bare_Basic_Decl (Node));
      end if;
   end Handle_Unit_Body;

end Libadalang.Unit_Files.Env_Hook;
