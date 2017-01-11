package body Libadalang.Unit_Files is

   ----------------
   -- Fetch_Unit --
   ----------------

   function Fetch_Unit
     (Ctx  : Analysis_Context;
      Name : Ada_Node;
      Kind : Unit_Kind) return Analysis_Unit
   is
      UFP      : constant Unit_File_Provider_Access_Cst :=
         Unit_File_Provider (Ctx);
      Name_Str : constant String := UFP.Get_File (Name, Kind);

      --  TODO??? Find a proper way to handle file not found, parsing error,
      --  etc.
      Unit : Analysis_Unit := Get_From_File (Ctx, Name_Str);
   begin
      if Root (Unit) /= null then
         Populate_Lexical_Env (Unit);
         Reference_Unit (From => Get_Unit (Name), Referenced => Unit);
      end if;

      return Unit;
   end Fetch_Unit;

end Libadalang.Unit_Files;
