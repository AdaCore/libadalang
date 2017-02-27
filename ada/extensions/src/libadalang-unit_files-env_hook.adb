package body Libadalang.Unit_Files.Env_Hook is

   Std_Content : String :=
     "package Standard is" & ASCII.LF &
     "pragma Pure(Standard);" & ASCII.LF &
     "  type Boolean is (False, True);" & ASCII.LF &
     "  type Integer is range -(2 ** 31) .. +(2 ** 31 - 1);" & ASCII.LF &
     "  subtype Natural  is Integer range 0 .. +(2 ** 31 - 1);" & ASCII.LF &
     "  subtype Positive is Integer range 1 .. +(2 ** 31 - 1);" & ASCII.LF &
     "  type Short_Short_Integer is range -(2 ** 7) .. +(2 ** 7 - 1);"
     & ASCII.LF &
     "  type Short_Integer       is range -(2 ** 15) .. +(2 ** 15 - 1);"
     & ASCII.LF &
     "  type Long_Integer        is range -(2 ** 31) .. +(2 ** 31 - 1);"
     & ASCII.LF &
     "  type Long_Long_Integer   is range -(2 ** 63) .. +(2 ** 63 - 1);"
     & ASCII.LF &
     "  type Short_Float     is digits 6" & ASCII.LF &
     "    range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;" & ASCII.LF &
     "  type Float           is digits 6" & ASCII.LF &
     "    range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;" & ASCII.LF &
     "  type Long_Float      is digits 15" & ASCII.LF &
     "    range -16#0.FFFF_FFFF_FFFF_F8#E+256 .. 16#0.FFFF_FFFF_FFFF_F8#E+256;"
     & ASCII.LF &
     "  type Long_Long_Float is digits 18" & ASCII.LF &
     "    range -16#0.FFFF_FFFF_FFFF_FFFF#E+4096 .. " & ASCII.LF &
     "16#0.FFFF_FFFF_FFFF_FFFF#E+4096;" & ASCII.LF &
     "  package ASCII is" & ASCII.LF &
     "     NUL   : constant Character := Character'Val (16#00#);" & ASCII.LF &
     "     SOH   : constant Character := Character'Val (16#01#);" & ASCII.LF &
     "     STX   : constant Character := Character'Val (16#02#);" & ASCII.LF &
     "     ETX   : constant Character := Character'Val (16#03#);" & ASCII.LF &
     "     EOT   : constant Character := Character'Val (16#04#);" & ASCII.LF &
     "     ENQ   : constant Character := Character'Val (16#05#);" & ASCII.LF &
     "     ACK   : constant Character := Character'Val (16#06#);" & ASCII.LF &
     "     BEL   : constant Character := Character'Val (16#07#);" & ASCII.LF &
     "     BS    : constant Character := Character'Val (16#08#);" & ASCII.LF &
     "     HT    : constant Character := Character'Val (16#09#);" & ASCII.LF &
     "     LF    : constant Character := Character'Val (16#0A#);" & ASCII.LF &
     "     VT    : constant Character := Character'Val (16#0B#);" & ASCII.LF &
     "     FF    : constant Character := Character'Val (16#0C#);" & ASCII.LF &
     "     CR    : constant Character := Character'Val (16#0D#);" & ASCII.LF &
     "     SO    : constant Character := Character'Val (16#0E#);" & ASCII.LF &
     "     SI    : constant Character := Character'Val (16#0F#);" & ASCII.LF &
     "     DLE   : constant Character := Character'Val (16#10#);" & ASCII.LF &
     "     DC1   : constant Character := Character'Val (16#11#);" & ASCII.LF &
     "     DC2   : constant Character := Character'Val (16#12#);" & ASCII.LF &
     "     DC3   : constant Character := Character'Val (16#13#);" & ASCII.LF &
     "     DC4   : constant Character := Character'Val (16#14#);" & ASCII.LF &
     "     NAK   : constant Character := Character'Val (16#15#);" & ASCII.LF &
     "     SYN   : constant Character := Character'Val (16#16#);" & ASCII.LF &
     "     ETB   : constant Character := Character'Val (16#17#);" & ASCII.LF &
     "     CAN   : constant Character := Character'Val (16#18#);" & ASCII.LF &
     "     EM    : constant Character := Character'Val (16#19#);" & ASCII.LF &
     "     SUB   : constant Character := Character'Val (16#1A#);" & ASCII.LF &
     "     ESC   : constant Character := Character'Val (16#1B#);" & ASCII.LF &
     "     FS    : constant Character := Character'Val (16#1C#);" & ASCII.LF &
     "     GS    : constant Character := Character'Val (16#1D#);" & ASCII.LF &
     "     RS    : constant Character := Character'Val (16#1E#);" & ASCII.LF &
     "     US    : constant Character := Character'Val (16#1F#);" & ASCII.LF &
     "     DEL   : constant Character := Character'Val (16#7F#);" & ASCII.LF &
     "     Exclam     : constant Character := '!';" & ASCII.LF &
     "     Quotation  : constant Character := '""';" & ASCII.LF &
     "     Sharp      : constant Character := '#';" & ASCII.LF &
     "     Dollar     : constant Character := '$';" & ASCII.LF &
     "     Percent    : constant Character := '%';" & ASCII.LF &
     "     Ampersand  : constant Character := '&';" & ASCII.LF &
     "     Colon      : constant Character := ':';" & ASCII.LF &
     "     Semicolon  : constant Character := ';';" & ASCII.LF &
     "     Query      : constant Character := '?';" & ASCII.LF &
     "     At_Sign    : constant Character := '@';" & ASCII.LF &
     "     L_Bracket  : constant Character := '[';" & ASCII.LF &
     "     Back_Slash : constant Character := '\';" & ASCII.LF &
     "     R_Bracket  : constant Character := ']';" & ASCII.LF &
     "     Circumflex : constant Character := '^';" & ASCII.LF &
     "     Underline  : constant Character := '_';" & ASCII.LF &
     "     Grave      : constant Character := '`';" & ASCII.LF &
     "     L_Brace    : constant Character := '{';" & ASCII.LF &
     "     Bar        : constant Character := '|';" & ASCII.LF &
     "     R_Brace    : constant Character := '}';" & ASCII.LF &
     "     Tilde      : constant Character := '~';" & ASCII.LF &
     "     LC_A : constant Character := 'a';" & ASCII.LF &
     "     LC_B : constant Character := 'b';" & ASCII.LF &
     "     LC_C : constant Character := 'c';" & ASCII.LF &
     "     LC_D : constant Character := 'd';" & ASCII.LF &
     "     LC_E : constant Character := 'e';" & ASCII.LF &
     "     LC_F : constant Character := 'f';" & ASCII.LF &
     "     LC_G : constant Character := 'g';" & ASCII.LF &
     "     LC_H : constant Character := 'h';" & ASCII.LF &
     "     LC_I : constant Character := 'i';" & ASCII.LF &
     "     LC_J : constant Character := 'j';" & ASCII.LF &
     "     LC_K : constant Character := 'k';" & ASCII.LF &
     "     LC_L : constant Character := 'l';" & ASCII.LF &
     "     LC_M : constant Character := 'm';" & ASCII.LF &
     "     LC_N : constant Character := 'n';" & ASCII.LF &
     "     LC_O : constant Character := 'o';" & ASCII.LF &
     "     LC_P : constant Character := 'p';" & ASCII.LF &
     "     LC_Q : constant Character := 'q';" & ASCII.LF &
     "     LC_R : constant Character := 'r';" & ASCII.LF &
     "     LC_S : constant Character := 's';" & ASCII.LF &
     "     LC_T : constant Character := 't';" & ASCII.LF &
     "     LC_U : constant Character := 'u';" & ASCII.LF &
     "     LC_V : constant Character := 'v';" & ASCII.LF &
     "     LC_W : constant Character := 'w';" & ASCII.LF &
     "     LC_X : constant Character := 'x';" & ASCII.LF &
     "     LC_Y : constant Character := 'y';" & ASCII.LF &
     "     LC_Z : constant Character := 'z';" & ASCII.LF &
     "  end ASCII;" & ASCII.LF &
     "  type String is array (Positive range <>) of Character;" & ASCII.LF &
     "  pragma Pack (String);" & ASCII.LF &
     "  type Wide_String is array " & ASCII.LF &
     "(Positive range <>) of Wide_Character;" & ASCII.LF &
     "  pragma Pack (Wide_String);" & ASCII.LF &
     "  type Duration is delta 0.000000001" & ASCII.LF &
     "    range -((2 ** 63 - 1) * 0.000000001) .." & ASCII.LF &
     "          +((2 ** 63 - 1) * 0.000000001);" & ASCII.LF &
     "  for Duration'Small use 0.000000001;" & ASCII.LF &
     "  Constraint_Error : exception;" & ASCII.LF &
     "  Program_Error    : exception;" & ASCII.LF &
     "  Storage_Error    : exception;" & ASCII.LF &
     "  Tasking_Error    : exception;" & ASCII.LF &
     "end Standard;" & ASCII.LF;

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : Name_List);
   --  Helper for the environment hook to handle WithDecl nodes

   procedure Handle_Unit_Decl (Ctx : Analysis_Context; Node : Basic_Decl);
   --  Helper for the environment hook to handle library-level unit decl nodes

   procedure Handle_Unit_Body (Ctx : Analysis_Context; Node : Body_Node);
   --  Helper for the environment hook to handle library-level unit body nodes

   --------------
   -- Env_Hook --
   --------------

   procedure Env_Hook (Unit : Analysis_Unit; Node : Ada_Node) is
      Ctx : constant Analysis_Context := Get_Context (Unit);
   begin
      if Node.all in With_Clause_Type'Class then
         Handle_With_Decl (Ctx, With_Clause (Node).F_Packages);
      elsif Node.all in Compilation_Unit_Type'Class then
         if not Has_Unit (Ctx, "standard.ads")
            and then Get_Filename (Get_Unit (Node)) /= "standard.ads"
         then
            declare
               Std : Analysis_Unit :=
                 Get_From_Buffer (Ctx, "standard.ads", "", Std_Content);
            begin
               Populate_Lexical_Env (Std);
            end;
         end if;
      elsif Node.Parent.all in Library_Item_Type'Class then
         if Node.all in Body_Node_Type'Class then
            Handle_Unit_Body (Ctx, Body_Node (Node));
         elsif Node.all in Basic_Decl_Type'Class then
            Handle_Unit_Decl (Ctx, Basic_Decl (Node));
         end if;
      end if;
   end Env_Hook;

   ----------------------
   -- Handle_With_Decl --
   ----------------------

   procedure Handle_With_Decl (Ctx : Analysis_Context; Names : Name_List) is
   begin
      for N of Names.Children loop
         declare
            Dummy : constant Analysis_Unit :=
               Fetch_Unit (Ctx, N, Unit_Specification);
         begin
            null;
         end;
      end loop;
   end Handle_With_Decl;

   ----------------------
   -- Handle_Unit_Decl --
   ----------------------

   procedure Handle_Unit_Decl (Ctx : Analysis_Context; Node : Basic_Decl) is
      Names : Name_Array_Access;
   begin
      --  If this not a library-level subprogram/package decl, there is no
      --  parent spec to process.
      if Node.all not in 
         Package_Decl_Type'Class
         | Basic_Subp_Decl_Type'Class
         | Generic_Package_Decl_Type'Class
         | Generic_Subp_Decl_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;
      pragma Assert (Names.N = 1);

      declare
         N : constant Ada_Node := Ada_Node (Names.Items (1));
      begin
         Dec_Ref (Names);
         if N.all in Dotted_Name_Type'Class then
            declare
               Dummy : constant Analysis_Unit := Fetch_Unit
                 (Ctx,
                  Ada_Node (Dotted_Name (N).F_Prefix),
                  Unit_Specification);
            begin
               null;
            end;
         end if;
      end;
   end Handle_Unit_Decl;

   ----------------------
   -- Handle_Unit_Body --
   ----------------------

   procedure Handle_Unit_Body (Ctx : Analysis_Context; Node : Body_Node) is
      Names : Name_Array_Access;
   begin
      --  If this not a library-level subprogram/package body, there is no spec
      --  to process.
      if Node.all not in Package_Body_Type'Class
         and then Node.all not in Subp_Body_Type'Class
      then
         return;
      end if;

      Names := Node.P_Defining_Names;
      pragma Assert (Names.N = 1);

      declare
         N     : constant Ada_Node := Ada_Node (Names.Items (1));
         Dummy : Analysis_Unit;
      begin
         Dec_Ref (Names);
         Dummy := Fetch_Unit (Ctx, N, Unit_Specification);
      end;
   end Handle_Unit_Body;

end Libadalang.Unit_Files.Env_Hook;
