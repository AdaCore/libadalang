--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with GNATCOLL.GMP; use GNATCOLL.GMP;
with GNATCOLL.GMP.Integers;

with Langkit_Support.Types; use Langkit_Support.Types;

with Libadalang.Analysis;          use Libadalang.Analysis;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Sources;           use Libadalang.Sources;
with Libadalang.Target_Info;       use Libadalang.Target_Info;

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
       := [Integer_IO'Access, Modular_IO'Access, Float_IO'Access,
           Fixed_IO'Access, Decimal_IO'Access, Enumeration_IO'Access];

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
            return [Get_Symbol (Name)];

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
      --  ``Unit``/``PLE_Root_Index``.

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
         Populate_Lexical_Env (Wrap_Unit (Unit), PLE_Root_Index);
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
                     --  we wanted to return, but we actually need to run PLE
                     --  on the renamed unit as well to ensure that entities
                     --  from the renamed package are visible from the
                     --  requesting unit.

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
                     Dec_Ref (Internal_Name);
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
      use GNATCOLL.GMP.Integers;
      subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;

      TI : Target_Information renames Context.Target_Info;

      Buffer : Unbounded_String;
      --  Source buffer for the Standard package to create

      --  Helpers to append formatted content to Buffer.
      --
      --  Level tracks the indentation level, which Indent/Dedent
      --  increases/decreases.
      --
      --  Line appends to Buffer a line indented according to Level.

      Trace_Lines : constant Boolean := Trace.Is_Active;
      Level       : Natural := 0;
      procedure Indent;
      procedure Dedent;
      procedure Line (S : String := "");

      procedure Type_Decl (Name, Def : String);
      --  Append a type declaration to Buffer. Name is the type name and Def is
      --  whatever follows the "is" keyword.

      procedure Integer_Type_Decl (Name : String; Size : Positive);
      --  Append a base integer type declaration for the given Name and Size

      procedure Float_Type_Decl
        (Name : String; Info : Floating_Point_Type_Information);
      --  Append a floating point type declaration that matches the given
      --  description.

      procedure Char_Constant (Name : String; Value : Character);
      --  Append a Character constant declaration for the given Name and Value

      ------------
      -- Indent --
      ------------

      procedure Indent is
      begin
         Level := Level + 1;
      end Indent;

      ------------
      -- Dedent --
      ------------

      procedure Dedent is
      begin
         Level := Level - 1;
      end Dedent;

      ----------
      -- Line --
      ----------

      procedure Line (S : String := "") is
      begin
         if S = "" then
            if Trace_Lines then
               Trace.Trace ("");
            end if;
         else
            declare
               Indentation : constant String := (1 .. 3 * Level => ' ');
            begin
               if Trace_Lines then
                  Trace.Trace (Indentation & S);
               end if;
               Append (Buffer, Indentation);
               Append (Buffer, S);
            end;
         end if;
         Append (Buffer, ASCII.LF);
      end Line;

      ---------------
      -- Type_Decl --
      ---------------

      procedure Type_Decl (Name, Def : String) is
      begin
         Line ("type " & Name & " is " & Def & ";");
      end Type_Decl;

      -----------------------
      -- Integer_Type_Decl --
      -----------------------

      procedure Integer_Type_Decl (Name : String; Size : Positive) is
         Base  : constant Big_Integer := Make ("2");
         Exp   : constant Unsigned_Long := Unsigned_Long (Size - 1);
         First : constant Big_Integer := -(Base ** Exp);
         Last  : constant Big_Integer := Base ** Exp - 1;
      begin
         Type_Decl
           (Name,
            "range " & Image (Encode_Integer_Literal (First, Base => 16))
            & " .. " & Image (Encode_Integer_Literal (Last, Base => 16)));
      end Integer_Type_Decl;

      ---------------------
      -- Float_Type_Decl --
      ---------------------

      procedure Float_Type_Decl
        (Name : String; Info : Floating_Point_Type_Information) is
      begin
         Type_Decl (Name, Image (Ada_Type_Definition (Info)));
      end Float_Type_Decl;

      -------------------
      -- Char_Constant --
      -------------------

      procedure Char_Constant (Name : String; Value : Character) is
      begin
         Line (Name & " : constant Character := Character'Val ("
               & Character'Pos (Value)'Image & ");");
      end Char_Constant;

      Std : Internal_Unit;
   begin
      if Trace_Lines then
         Trace.Trace ("== Synthetized Standard package code ==");
      end if;

      --  The content we generate for the Standard package is inspired by
      --  GNAT's output for the -gnatS compilation switch, adapted for the
      --  target information stored in Context.

      Line ("package Standard is");
      Indent;

      Line ("pragma Pure (Standard);");
      Line;

      Type_Decl ("Boolean", "(False, True)");
      Line;

      Integer_Type_Decl ("Integer", TI.Int_Size);
      Line ("subtype Natural is Integer range 0 .. Integer'Last;");
      Line ("subtype Positive is Integer range 1 .. Integer'Last;");
      Line;

      Integer_Type_Decl ("Short_Short_Integer", TI.Char_Size);
      Integer_Type_Decl ("Short_Integer", TI.Short_Size);
      Integer_Type_Decl ("Long_Integer", TI.Long_Size);
      Integer_Type_Decl ("Long_Long_Integer", TI.Long_Long_Size);
      Integer_Type_Decl ("Long_Long_Long_Integer", TI.Long_Long_Long_Size);
      Line;

      Float_Type_Decl ("Short_Float", TI.Floating_Point_Types (Float_Id));
      Float_Type_Decl ("Float", TI.Floating_Point_Types (Float_Id));
      Float_Type_Decl ("Long_Float", TI.Floating_Point_Types (Double_Id));
      Float_Type_Decl ("Long_Long_Float",
                       TI.Floating_Point_Types (Long_Double_Id));
      Line;

      Type_Decl ("Character", "('A')");
      Type_Decl ("Wide_Character", "('A')");
      Type_Decl ("Wide_Wide_Character", "('A')");
      Line;

      Line ("package ASCII is");
      Indent;
      Char_Constant ("NUL", ASCII.NUL);
      Char_Constant ("SOH", ASCII.SOH);
      Char_Constant ("STX", ASCII.STX);
      Char_Constant ("ETX", ASCII.ETX);
      Char_Constant ("EOT", ASCII.EOT);
      Char_Constant ("ENQ", ASCII.ENQ);
      Char_Constant ("ACK", ASCII.ACK);
      Char_Constant ("BEL", ASCII.BEL);
      Char_Constant ("BS", ASCII.BS);
      Char_Constant ("HT", ASCII.HT);
      Char_Constant ("LF", ASCII.LF);
      Char_Constant ("VT", ASCII.VT);
      Char_Constant ("FF", ASCII.FF);
      Char_Constant ("CR", ASCII.CR);
      Char_Constant ("SO", ASCII.SO);
      Char_Constant ("SI", ASCII.SI);
      Char_Constant ("DLE", ASCII.DLE);
      Char_Constant ("DC1", ASCII.DC1);
      Char_Constant ("DC2", ASCII.DC2);
      Char_Constant ("DC3", ASCII.DC3);
      Char_Constant ("DC4", ASCII.DC4);
      Char_Constant ("NAK", ASCII.NAK);
      Char_Constant ("SYN", ASCII.SYN);
      Char_Constant ("ETB", ASCII.ETB);
      Char_Constant ("CAN", ASCII.CAN);
      Char_Constant ("EM", ASCII.EM);
      Char_Constant ("SUB", ASCII.SUB);
      Char_Constant ("ESC", ASCII.ESC);
      Char_Constant ("FS", ASCII.FS);
      Char_Constant ("GS", ASCII.GS);
      Char_Constant ("RS", ASCII.RS);
      Char_Constant ("US", ASCII.US);
      Char_Constant ("DEL", ASCII.DEL);
      Line;

      Char_Constant ("Exclam", '!');
      Char_Constant ("Quotation", '"');
      Char_Constant ("Sharp",  '#');
      Char_Constant ("Dollar", '$');
      Char_Constant ("Percent", '%');
      Char_Constant ("Ampersand", '&');
      Char_Constant ("Colon",  ':');
      Char_Constant ("Semicolon", ';');
      Char_Constant ("Query",  '?');
      Char_Constant ("At_Sign", '@');
      Char_Constant ("L_Bracket", '[');
      Char_Constant ("Back_Slash", '\');
      Char_Constant ("R_Bracket", ']');
      Char_Constant ("Circumflex", '^');
      Char_Constant ("Underline", '_');
      Char_Constant ("Grave", '`');
      Char_Constant ("L_Brace", '{');
      Char_Constant ("Bar", '|');
      Char_Constant ("R_Brace", '}');
      Char_Constant ("Tilde",  '~');
      Char_Constant ("LC_A", 'a');
      Char_Constant ("LC_B", 'b');
      Char_Constant ("LC_C", 'c');
      Char_Constant ("LC_D", 'd');
      Char_Constant ("LC_E", 'e');
      Char_Constant ("LC_F", 'f');
      Char_Constant ("LC_G", 'g');
      Char_Constant ("LC_H", 'h');
      Char_Constant ("LC_I", 'i');
      Char_Constant ("LC_J", 'j');
      Char_Constant ("LC_K", 'k');
      Char_Constant ("LC_L", 'l');
      Char_Constant ("LC_M", 'm');
      Char_Constant ("LC_N", 'n');
      Char_Constant ("LC_O", 'o');
      Char_Constant ("LC_P", 'p');
      Char_Constant ("LC_Q", 'q');
      Char_Constant ("LC_R", 'r');
      Char_Constant ("LC_S", 's');
      Char_Constant ("LC_T", 't');
      Char_Constant ("LC_U", 'u');
      Char_Constant ("LC_V", 'v');
      Char_Constant ("LC_W", 'w');
      Char_Constant ("LC_X", 'x');
      Char_Constant ("LC_Y", 'y');
      Char_Constant ("LC_Z", 'z');
      Dedent;
      Line ("end ASCII;");
      Line;

      Type_Decl ("String", "array (Positive range <>) of Character");
      Line ("pragma Pack (String);");
      Type_Decl ("Wide_String", "array (Positive range <>) of Wide_Character");
      Line ("pragma Pack (Wide_String);");
      Type_Decl ("Wide_Wide_String",
                 "array (Positive range <>) of Wide_Wide_Character");
      Line;

      declare
         Duration_32_Bits_On_Target : constant Boolean :=
           TI.Long_Long_Long_Size < 64;

         Duration_Delta : constant String :=
           (if Duration_32_Bits_On_Target
            then "0.020"
            else "0.000000001");
         Exponent       : constant String :=
           (if Duration_32_Bits_On_Target
            then "31"
            else "63");
      begin
         Type_Decl
           ("Duration",
            "delta " & Duration_Delta & " range"
            & " -((2 ** " & Exponent & ") * " & Duration_Delta & ") .."
            & " +((2 ** " & Exponent & " - 1) * " & Duration_Delta & ")");
         Line ("for Duration'Small use " & Duration_Delta & ";");
      end;
      Line;

      Type_Decl ("Universal_Int_Type_",  "range -1 .. 1");
      Type_Decl ("Universal_Real_Type_", "digits 16");
      Type_Decl ("Universal_Fixed_Type_", "delta 0.01 range -1.0 .. 1.0");
      Line;

      Line ("package root_types_ is");
      Indent;
      Type_Decl ("root_integer",  "range -1 .. 1");
      Type_Decl ("root_real", "digits 16");
      Dedent;
      Line ("end root_types_;");
      Line;

      Line ("Constraint_Error : exception;");
      Line ("Numeric_Error    : exception;");
      Line ("Program_Error    : exception;");
      Line ("Storage_Error    : exception;");
      Line ("Tasking_Error    : exception;");
      Line ("Abort_Signal_    : exception;");

      Dedent;
      Line ("end Standard;");

      declare
         Bytes       : Big_String_Access;
         Bytes_Count : Natural;
      begin
         Get_String (Buffer, Bytes, Bytes_Count);

         Std := Get_Unit
           (Context     => Context,
            Filename    => "__standard",
            Charset     => "ascii",
            Reparse     => True,
            Input       => (Kind        => Bytes_Buffer,
                            Charset     => To_Unbounded_String ("ascii"),
                            Read_BOM    => False,
                            Bytes       => Bytes.all'Address,
                            Bytes_Count => Bytes_Count),
            Rule        => Default_Grammar_Rule,
            Is_Internal => True);

         --  If Buffer contains a parsing error, the code above has a serious
         --  bug: do not let it go unnoticed.

         if not Std.Diagnostics.Is_Empty then
            raise Program_Error with "parsing error in Standard";
         end if;
      end;

      Populate_Lexical_Env (Std, 1);
   end Fetch_Standard;

end Libadalang.Env_Hooks;
