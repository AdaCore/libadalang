--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Analysis;          use Libadalang.Analysis;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Env_Hooks is

   use Support.Text;

   Text_IO           : constant Text_Type := "ada.text_io";
   Wide_Text_IO      : constant Text_Type := "ada.wide_text_io";
   Wide_Wide_Text_IO : constant Text_Type := "ada.wide_wide_text_io";

   Integer_IO     : aliased constant Text_Type := "integer_io";
   Modular_IO     : aliased constant Text_Type := "modular_io";
   Float_IO       : aliased constant Text_Type := "float_io";
   Fixed_IO       : aliased constant Text_Type := "fixed_io";
   Decimal_IO     : aliased constant Text_Type := "decimal_io";
   Enumeration_IO : aliased constant Text_Type := "enumeration_io";

   Text_IO_Subpackages :
     constant array (Positive range <>) of access constant Text_Type
       := (Integer_IO'Access, Modular_IO'Access, Float_IO'Access,
           Fixed_IO'Access, Decimal_IO'Access, Enumeration_IO'Access);

   --  The content of the following string literal has been generated running
   --  GNAT with flag -gnatS, and then post-processed by hand.

   Std_Content : constant String :=
     "package Standard is" & ASCII.LF &
     "pragma Pure(Standard);" & ASCII.LF &
     "  type Boolean is (False, True);" & ASCII.LF &
     "  type Integer is range -(2 ** 31) .. +(2 ** 31 - 1);" & ASCII.LF &
     "  subtype Natural  is Integer range 0 .. +(2 ** 31 - 1);" & ASCII.LF &
     "  subtype Positive is Integer range 1 .. +(2 ** 31 - 1);" & ASCII.LF &
     "  type Short_Short_Integer    is range -(2 ** 7) .. +(2 ** 7 - 1);"
     & ASCII.LF &
     "  type Short_Integer          is range -(2 ** 15) .. +(2 ** 15 - 1);"
     & ASCII.LF &
     "  type Long_Integer           is range -(2 ** 31) .. +(2 ** 31 - 1);"
     & ASCII.LF &
     "  type Long_Long_Integer      is range -(2 ** 63) .. +(2 ** 63 - 1);"
     & ASCII.LF &
     "  type Long_Long_Long_Integer is range -(2 ** 127) .. +(2 ** 127 - 1);"
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
     "  type Universal_Int_Type_ is range -1 .. 1;" & ASCII.LF &
     "  type Universal_Real_Type_ is digits 16;" & ASCII.LF &
     "  type Universal_Fixed_Type_ is delta 0.01 range -1.0 .. 1.0;"
     & ASCII.LF &
     "  package root_types_ is" & ASCII.LF &
     "  type root_integer is range -1 .. 1;" & ASCII.LF &
     "  type root_real is digits 16;" & ASCII.LF & ASCII.LF &
     "  end root_types_;" & ASCII.LF &
     "  Constraint_Error : exception;" & ASCII.LF &
     "  Numeric_Error    : exception;" & ASCII.LF &
     "  Program_Error    : exception;" & ASCII.LF &
     "  Storage_Error    : exception;" & ASCII.LF &
     "  Tasking_Error    : exception;" & ASCII.LF &
     "  Abort_Signal_    : exception;" & ASCII.LF &
     "end Standard;" & ASCII.LF;

   ---------------------
   -- Name_To_Symbols --
   ---------------------

   function Name_To_Symbols (Name : Bare_Name) return Symbol_Type_Array is
   begin
      if Name = null then
         raise Property_Error with "fatal parsing error in Name_To_Symbols";
      end if;

      case Defining_Name_Nodes (Name.Kind) is
         when Ada_Base_Id =>
            return (1 => Get_Symbol (Name));

         when Ada_Dotted_Name =>
            return Name_To_Symbols (Name.Dotted_Name_F_Prefix)
                   & Name_To_Symbols (Name.Dotted_Name_F_Suffix);

         when Ada_Defining_Name =>
            return Name_To_Symbols (Name.Defining_Name_F_Name);
      end case;
   end Name_To_Symbols;

   ---------------
   -- To_String --
   ---------------

   function To_String (Name : Symbol_Type_Array) return Text_Type
   is
      (if Name'Length > 0
       then +Name (Name'First)
            & (if Name'Length > 1
               then "." & To_String (Name (Name'First + 1 .. Name'Last))
               else "")
       else "");

   ----------------
   -- Fetch_Unit --
   ----------------

   function Fetch_Unit
     (Ctx                : Internal_Context;
      Name               : Symbol_Type_Array;
      Kind               : Analysis_Unit_Kind;
      From_Unit          : Internal_Unit;
      Load_If_Needed     : Boolean;
      Do_Prepare_Nameres : Boolean := True;
      Not_Found_Is_Error : Boolean := False;
      Process_Parents    : Boolean := True) return Internal_Unit
   is

      procedure Prepare_Nameres
        (Unit : Internal_Unit; PLE_Root_Index : Positive);
      --  Prepare semantic analysis for the compilation unit at
      --  ``Unit``/``PLE_Root_Index`` and add a reference from ``From_Unit`` to
      --  ``Unit``.

      procedure Emit_Unit_Requested
        (Unit               : Internal_Unit;
         Not_Found_Is_Error : Boolean);
      --  If there is an event handler, invoke its ``Unit_Requested_Callback``
      --  event for ``Unit``. ``Not_Found_Is_Error`` is forwarded as-is to the
      --  callback.

      ---------------------
      -- Prepare_Nameres --
      ---------------------

      procedure Prepare_Nameres
        (Unit : Internal_Unit; PLE_Root_Index : Positive) is
      begin
         if Unit.Ast_Root /= null then
            Populate_Lexical_Env (Wrap_Unit (Unit), PLE_Root_Index);
            Reference_Unit (From       => From_Unit,
                            Referenced => Unit);
         end if;
      end Prepare_Nameres;

      -------------------------
      -- Emit_Unit_Requested --
      -------------------------

      procedure Emit_Unit_Requested
        (Unit               : Internal_Unit;
         Not_Found_Is_Error : Boolean) is
      begin
         --  TODO??? We now handle file not found via
         --  ``Unit_Requested_Callback``, but we don't really handle parsing
         --  errors directly. Do we need to do something more? Or can we
         --  consider that anything can be done in the callback anyway?

         if Ctx.Event_Handler /= null then

            --  TODO??? (libadalang#1028) Passing a filename as a Text_Type is
            --  dubious. Do the best approximation we can without crashing for
            --  non-ASCII bytes for now, but in the future we may want to
            --  change the signature for this event.

            declare
               Filename : constant String := Get_Filename (Unit);
               Name     : Text_Type (Filename'Range);
            begin
               for I in Filename'Range loop
                  declare
                     C : Character renames Filename (I);
                  begin
                     Name (I) :=
                       (if C in Character'Val (0) .. Character'Val (127)
                        then Wide_Wide_Character'Val (Character'Pos (C))
                        else '?');
                  end;
               end loop;
               Ctx.Event_Handler.Unit_Requested_Callback
                 (Ctx,
                  Name,
                  From_Unit,
                  Unit.Ast_Root /= null,
                  Not_Found_Is_Error);
            end;
         end if;
      end Emit_Unit_Requested;

      Unit_Name      : constant Text_Type := To_String (Name);
      Unit           : Internal_Unit;
      PLE_Root_Index : Positive;
   begin
      --  If we must not load missing units and this one is missing, do
      --  nothing.

      if not Load_If_Needed then
         declare
            Filename : String_Access;
         begin
            Get_Unit_Location
              (Context        => Ctx,
               Name           => Unit_Name,
               Kind           => Kind,
               Filename       => Filename,
               PLE_Root_Index => PLE_Root_Index);
            if not Has_Unit (Ctx, Filename.all) then
               return null;
            end if;
         end;
      end if;

      --  If we are not preparing nameres, we can directly return the unit
      --  corresponding to the entire name.

      if not Do_Prepare_Nameres then
         Get_Unit_And_PLE_Root
           (Context        => Ctx,
            Name           => Unit_Name,
            Kind           => Kind,
            Unit           => Unit,
            PLE_Root_Index => PLE_Root_Index);
         return Unit;
      end if;

      --  GNAT kludge: as an "optimization", the generic subpackages in
      --  Ada.Text_IO (see Text_IO_Subpackages) are not present in the
      --  Ada.Text_IO unit itself, but in private child packages. GNAT
      --  magically imports them in Ada.Text_IO's namespace.
      --
      --  Here, try to import these child units as soon as someone WITHes
      --  Ada.Text_IO.

      if Kind = Unit_Specification
         and then Unit_Name in Text_IO | Wide_Text_IO | Wide_Wide_Text_IO
      then
         for SP of Text_IO_Subpackages loop
            declare
               SP_Symbol : constant Symbol_Type :=
                  Lookup_Symbol (Ctx, SP.all);
               SP_FQN    : constant Symbol_Type_Array := Name & SP_Symbol;
            begin
               Get_Unit_And_PLE_Root
                 (Context        => Ctx,
                  Name           => To_String (SP_FQN),
                  Kind           => Kind,
                  Unit           => Unit,
                  PLE_Root_Index => PLE_Root_Index);
               Prepare_Nameres (Unit, PLE_Root_Index);
            end;
         end loop;
      end if;

      --  If we should load only the unit that ``Name`` and ``Kind`` designate,
      --  return it now and return. Do not forget to emit the "unit requested
      --  callback" event.

      if not Process_Parents then
         Get_Unit_And_PLE_Root
           (Context        => Ctx,
            Name           => Unit_Name,
            Kind           => Kind,
            Unit           => Unit,
            PLE_Root_Index => PLE_Root_Index);
         Emit_Unit_Requested (Unit, Not_Found_Is_Error);
         Prepare_Nameres (Unit, PLE_Root_Index);
         return Unit;
      end if;

      declare
         procedure Step
           (Name  : Symbol_Type_Array; Index : Positive);
         --  Step into each portion of ``Name``, resolving each unit
         --  incrementally.  This is a recursive procedure, that will resolve
         --  the name upwards from ``Name (Index)``.
         --
         --  This is a recursive procedure rather than a loop so that we can
         --  handle package renamings, and modify the currently examined name:
         --  For example, given the name ``("Text_IO", "Complex_IO")``, and
         --  given ``"Text_IO"`` designates a package renaming to
         --  ``Ada.Text_IO``, we will resolve the package renaming, and the
         --  first recursive call to ``Step`` will be
         --
         --  ``Step (("Ada", "Text_IO", "Complex_IO"), 3)`` where we
         --  substituted the renaming package to the renamed entity, and
         --  incremented the index accordingly.

         ----------
         -- Step --
         ----------

         procedure Step
           (Name  : Symbol_Type_Array;
            Index : Positive)
         is
            Is_Last : constant Boolean := Index = Name'Last;
            --  Whether this call to ``Step`` is the last one, i.e. the one to
            --  fetch the unit to return.

            Current_Name : constant Text_Type :=
              To_String (Name (Name'First .. Index));

            I_Kind : constant Analysis_Unit_Kind :=
              (if Is_Last then Kind else Unit_Specification);
            --  When looking for unit ``A.B``, ``A`` is a specification even if
            --  we mean to fetch ``B``'s body, unless ``B`` is a subunit (in
            --  that case ``A`` must have a body).
         begin
            Get_Unit_And_PLE_Root
              (Context        => Ctx,
               Name           => Current_Name,
               Kind           => I_Kind,
               Unit           => Unit,
               PLE_Root_Index => PLE_Root_Index);

            --  If we are trying to fetch a dependency of the requested unit,
            --  it may be a subunit: if fetching a spec did not work, try
            --  fetching a body instead.

            if not Is_Last and then Unit.Ast_Root = null then
               declare
                  B : Internal_Unit;
                  I : Positive;
               begin
                  Get_Unit_And_PLE_Root
                    (Context        => Ctx,
                     Name           => Current_Name,
                     Kind           => Unit_Body,
                     Unit           => B,
                     PLE_Root_Index => I);
                  Emit_Unit_Requested
                    (Unit => B, Not_Found_Is_Error => False);

                  --  Consider the body for the rest of the processing iff we
                  --  have found it, otherwise keep the spec unit in ``Unit``.

                  if B.Ast_Root /= null then
                     Unit := B;
                     PLE_Root_Index := I;
                  end if;
               end;
            end if;

            --  Consider that a missing unit is an error if
            --  ``Not_Found_Is_Error`` or if ``Unit`` is not the requested unit
            --  (i.e. just another unit in the closure).

            Emit_Unit_Requested
              (Unit               => Unit,
               Not_Found_Is_Error => not Is_Last or else Not_Found_Is_Error);

            Prepare_Nameres (Unit, PLE_Root_Index);

            declare
               Internal_Name : Symbol_Type_Array_Access :=
                 Create_Symbol_Type_Array (Internal_Symbol_Type_Array (Name));

               Comp_Unit : constant Compilation_Unit := Wrap_Node
                 (Ada_Node_P_Compilation_Unit_With_Name
                    (Unit.Ast_Root, Unit, Internal_Name)).As_Compilation_Unit;

               Decl : constant Basic_Decl :=
                 (if Comp_Unit.Is_Null
                  then No_Basic_Decl
                  else Comp_Unit.P_Decl);

            begin
               if not Decl.Is_Null
                  and then Decl.Kind in
                    Libadalang.Common.Ada_Package_Renaming_Decl_Range
               then
                  --  If the declaration is a package renaming, resolve the
                  --  renamed package..

                  declare
                     Target        : constant Basic_Decl :=
                       Decl.As_Package_Renaming_Decl.P_Final_Renamed_Package;
                     Resolved_Name : Symbol_Type_Array_Access :=
                       Basic_Decl_P_Fully_Qualified_Name_Array
                         (Unwrap_Node (Target));
                     New_Index     : constant Positive :=
                       Resolved_Name.Items'Last;
                  begin
                     --  .. and make the next call to step consider the renamed
                     --  package.

                     Step
                       (Name  => Symbol_Type_Array (Resolved_Name.Items)
                                 & Name (Index + 1 .. Name'Last),
                        Index => New_Index);

                     --  However if that was the last part of the name, we
                     --  still want to return the renaming unit, and not
                     --  renamed one. In theory we could have returned before
                     --  resolving the renaming because we already had the unit
                     --  we wanted to return, but if we did that we would not
                     --  have updated the `Referenced_Units` vector to include
                     --  the fact that `From_Unit` references the renamed unit.

                     if Is_Last then
                        Unit := Unwrap_Unit (Comp_Unit.Unit);
                     end if;

                     Free (Resolved_Name);
                  exception
                     when Precondition_Failure | Property_Error =>
                        Free (Resolved_Name);
                        raise;
                  end;
               else
                  --  We're on the last portion of the name: return

                  if Is_Last then
                     return;
                  end if;

                  --  Else, just resolve the next portion of the given name

                  Step (Name, Index + 1);
               end if;

               Dec_Ref (Internal_Name);
            exception
               when Precondition_Failure | Property_Error =>
                  Free (Internal_Name);
                  raise;
            end;
         end Step;
      begin
         Step (Name, Name'First);
      end;

      return Unit;
   end Fetch_Unit;

   --------------------
   -- Fetch_Standard --
   --------------------

   procedure Fetch_Standard (Context : Internal_Context) is
      Std : constant Internal_Unit := Get_Unit
        (Context     => Context,
         Filename    => "__standard",
         Charset     => "ascii",
         Reparse     => True,
         Input       => (Kind        => Bytes_Buffer,
                         Charset     => To_Unbounded_String ("ascii"),
                         Read_BOM    => False,
                         Bytes       => Std_Content'Address,
                         Bytes_Count => Std_Content'Length),
         Rule        => Default_Grammar_Rule,
         Is_Internal => True);
   begin
      Populate_Lexical_Env (Std, 1);
   end Fetch_Standard;

end Libadalang.Env_Hooks;
