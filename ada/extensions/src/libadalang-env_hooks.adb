------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Converters;     use Libadalang.Converters;
with Libadalang.Implementation; use Libadalang.Implementation;

package body Libadalang.Env_Hooks is

   procedure Handle_Unit_With_Parents
     (Ctx : Internal_Context; Node : Bare_Basic_Decl);
   --  Helper for the environment hook to handle library-level unit decl nodes

   procedure Handle_Unit_Body (Ctx : Internal_Context; Node : Bare_Body_Node);
   --  Helper for the environment hook to handle library-level unit body nodes

   procedure Handle_Subunit (Ctx : Internal_Context; Node : Bare_Basic_Decl);
   --  Helper for the environment hook to handle sub-units (separates)

   Text_IO        : constant Text_Type := "ada.text_io";
   Integer_IO     : aliased constant Text_Type := "integer_io";  
   Float_IO       : aliased constant Text_Type := "float_io";
   Fixed_IO       : aliased constant Text_Type := "fixed_io";
   Decimal_IO     : aliased constant Text_Type := "decimal_io";
   Enumeration_IO : aliased constant Text_Type := "enumeration_io";

   Text_IO_Subpackages :
     constant array (Positive range <>) of access constant Text_Type 
       := (Integer_IO'Access, Float_IO'Access, Fixed_IO'Access,
           Decimal_IO'Access, Enumeration_IO'Access);

   --  The content of the following string literal has been generated running
   --  GNAT with flag -gnatS, and then post-processed by hand.

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
     "  type Character is ('A');" & ASCII.LF &
     "  type Wide_Character is ('A');" & ASCII.LF &
     "  type Wide_Wide_Character is ('A');" & ASCII.LF &
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
     "  type Wide_Wide_String is array " & ASCII.LF &
     "(Positive range <>) of Wide_Wide_Character;" & ASCII.LF &
     "  pragma Pack (Wide_String);" & ASCII.LF &
     "  type Duration is delta 0.000000001" & ASCII.LF &
     "    range -((2 ** 63 - 1) * 0.000000001) .." & ASCII.LF &
     "          +((2 ** 63 - 1) * 0.000000001);" & ASCII.LF &
     "  for Duration'Small use 0.000000001;" & ASCII.LF &
     "  Constraint_Error : exception;" & ASCII.LF &
     "  Program_Error    : exception;" & ASCII.LF &
     "  Storage_Error    : exception;" & ASCII.LF &
     "  Tasking_Error    : exception;" & ASCII.LF &
     "  type Universal_Int_Type_ is range -1 .. 1;" & ASCII.LF &
     "  type Universal_Real_Type_ is digits 16;" & ASCII.LF &
     "end Standard;" & ASCII.LF;

   ---------------------
   -- Name_To_Symbols --
   ---------------------

   function Name_To_Symbols 
      (Name : access Bare_Name_Type'Class) return Symbol_Type_Array 
   is
     (case Name.Kind is

      when Ada_Base_Id =>
        (1 => Get_Symbol (Bare_Identifier (Name))),

      when Ada_Dotted_Name =>
        Name_To_Symbols (Bare_Dotted_Name (Name).F_Prefix)
        & Name_To_Symbols (Bare_Dotted_Name (Name).F_Suffix),

      when Ada_Defining_Name =>
        Name_To_Symbols (Bare_Defining_Name (Name).F_Name),

      when others =>
         raise Property_Error with "Wrong node in Name_To_Symbols");

   ---------------
   -- To_String --
   ---------------

   function To_String (Name : Symbol_Type_Array) return Text_Type
   is
      (if Name'Length > 0
       then Name (Name'First).all
            & (if Name'Length > 1
               then "." & To_String (Name (Name'First + 1 .. Name'Last))
               else "")
       else "");

   ----------------
   -- Fetch_Unit --
   ----------------

   function Fetch_Unit
     (Ctx            : Internal_Context;
      Name           : Bare_Name;
      Kind           : Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit is
   begin
      return Fetch_Unit
        (Ctx, Name_To_Symbols (Name), 
         Bare_Ada_Node (Name).Unit, Kind,
         Load_If_Needed);
   end Fetch_Unit;

   function Fetch_Unit
     (Ctx            : Internal_Context;
      Name           : Symbol_Type_Array;
      From_Unit      : Internal_Unit;
      Kind           : Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit
   is
      procedure Prepare_Nameres (Unit : Internal_Unit);
      --  Prepare semantic analysis and reference Unit from the current unit

      ---------------------
      -- Prepare_Nameres --
      ---------------------

      procedure Prepare_Nameres (Unit : Internal_Unit) is
      begin
         if Unit.AST_Root /= null then
            Populate_Lexical_Env (Wrap_Unit (Unit));
            Reference_Unit (From       => From_Unit,
                            Referenced => Unit);
         end if;
      end Prepare_Nameres;

      UFP              : constant Internal_Unit_Provider_Access :=
         Ctx.Unit_Provider;
      Unit, First_Unit : Internal_Unit;
   begin
      if not Load_If_Needed then
         declare
            Filename : constant String :=
               UFP.Get_Unit_Filename (To_String (Name), Kind);
         begin
            if Filename = "" then
               return null;
            elsif not Has_Unit (Ctx, Filename) then
               return null;
            end if;
         end;
      end if;

      --  GNAT kludge: as an "optimization", the generic subpackages in
      --  Ada.Text_IO (see Text_IO_Subpackages) are not present in the
      --  Ada.Text_IO unit itself, but in private child packages. GNAT
      --  magically imports them in Ada.Text_IO's namespace.
      --
      --  Here, try to import these child unit as soon as someone WITHes
      --  Ada.Text_IO.

      if Kind = Unit_Specification and then To_String (Name) = Text_IO then
         for SP of Text_IO_Subpackages loop
            declare
               SP_Symbol : constant Symbol_Type :=
                  Lookup_Symbol (Ctx, SP.all);
               SP_FQN    : constant Symbol_Type_Array := Name & SP_Symbol;
            begin
               Prepare_Nameres
                 (UFP.Get_Unit (Ctx, To_String (SP_FQN), Kind));
            end;
         end loop;
      end if;

      --  In Ada, "with A.B" gives visibility to A and A.B. To process all
      --  "mentioned" units, the following loop iterates on ["A.B", "A"].

      for I in reverse Name'Range loop
         declare
            Current_Name : Symbol_Type_Array := Name (Name'First .. I);

            I_Kind : constant Unit_Kind :=
              (if I = Name'Last then Kind else Unit_Specification);
            --  When looking for unit A.B, A is a specification even if we mean
            --  to fetch B's body.
         begin
            --  TODO??? Find a proper way to handle file not found, parsing
            --  error, etc.
            Unit := UFP.Get_Unit (Ctx, To_String (Current_Name), I_Kind);
            Prepare_Nameres (Unit);

            --  The first iteration gives the unit we are required to return
            if First_Unit = null then
               First_Unit := Unit;
            end if;
         end;
      end loop;
      return First_Unit;
   end Fetch_Unit;

   --------------------
   -- Fetch_Standard --
   --------------------

   procedure Fetch_Standard (Context : Internal_Context) is
      Std : constant Analysis_Unit :=
        Get_From_Buffer (Wrap_Context (Context), "__standard",
                         "ascii", Std_Content);
   begin
      Populate_Lexical_Env (Std);
   end Fetch_Standard;

   --------------
   -- Env_Hook --
   --------------

   procedure Env_Hook (Unit : Internal_Unit; Node : Bare_Ada_Node) is
      Ctx : constant Internal_Context := Unit.Context;
   begin
      if Node.Parent.all in Bare_Library_Item_Type'Class then
         if Node.all in Bare_Body_Node_Type'Class then
            Handle_Unit_Body (Ctx, Bare_Body_Node (Node));
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
     (Ctx : Internal_Context; Node : Bare_Basic_Decl)
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

      N := Node.P_Defining_Name.Node.F_Name;

      if N.all in Bare_Dotted_Name_Type'Class then
         declare
            Dummy : constant Internal_Unit := Fetch_Unit
              (Ctx,
               Bare_Dotted_Name (N).F_Prefix,
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

   procedure Handle_Subunit (Ctx : Internal_Context; Node : Bare_Basic_Decl)
   is
      --  Sub-unit handling is very simple: We just want to fetch the
      --  containing unit.
      Dummy : constant Internal_Unit := Fetch_Unit
        (Ctx, Bare_Subunit (Node.Parent).F_Name, Unit_Body,
         Load_If_Needed => True);
   begin
      null;
   end Handle_Subunit;

   ----------------------
   -- Handle_Unit_Body --
   ----------------------

   procedure Handle_Unit_Body (Ctx : Internal_Context; Node : Bare_Body_Node)
   is
      Names : Internal_Entity_Defining_Name_Array_Access;
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
         N     : constant Bare_Name := Names.Items (1).Node.F_Name;
         Dummy : Internal_Unit;
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

end Libadalang.Env_Hooks;
