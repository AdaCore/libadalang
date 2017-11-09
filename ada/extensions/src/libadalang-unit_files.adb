with Langkit_Support.Text; use Langkit_Support.Text;

package body Libadalang.Unit_Files is

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

   ---------------------
   -- Name_To_Symbols --
   ---------------------

   function Name_To_Symbols (Name : Bare_Ada_Node) return Symbol_Type_Array is
     (case Kind (Name) is

      when Ada_Base_Id =>
        (1 => Get_Symbol (F_Tok (Bare_Identifier (Name)))),

      when Ada_Dotted_Name =>
        Name_To_Symbols (Bare_Ada_Node (Bare_Dotted_Name (Name).F_Prefix))
        & Name_To_Symbols
           (Bare_Ada_Node (Bare_Dotted_Name (Name).F_Suffix)),

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
     (Ctx  : Analysis_Context;
      Name : Bare_Ada_Node;
      Kind : Unit_Kind) return Analysis_Unit
   is
   begin
      return Fetch_Unit
        (Ctx, Name_To_Symbols (Name), Get_Unit (Create (Name)), Kind);
   end Fetch_Unit;

   function Fetch_Unit
     (Ctx       : Analysis_Context;
      Name      : Symbol_Type_Array;
      From_Unit : Analysis_Unit;
      Kind      : Unit_Kind) return Analysis_Unit
   is
      procedure Prepare_Semres (Unit : Analysis_Unit);
      --  Prepare semantic analysis and reference Unit from the current unit

      --------------------
      -- Prepare_Semres --
      --------------------

      procedure Prepare_Semres (Unit : Analysis_Unit) is
      begin
         if not Root (Unit).Is_Null then
            Populate_Lexical_Env (Unit);
            Reference_Unit (From       => From_Unit,
                            Referenced => Unit);
         end if;
      end Prepare_Semres;

      UFP : constant Unit_Provider_Access_Cst := Unit_Provider (Ctx);
      Unit, First_Unit : Analysis_Unit;
   begin

      --  GNAT kludge: as an "optimization", the generic subpackages in
      --  Ada.Text_IO (see Text_IO_Subpackages) are not present in the
      --  Ada.Text_IO unit itself, but in private child packages. GNAT
      --  magically imports them in Ada.Text_IO's namespace.
      --
      --  Here, try to import these child unit as soon as someone WITHes
      --  Ada.Text_IO.

      if Kind = Unit_Specification and then To_String (Name) = Text_IO then
         for SP of Text_IO_Subpackages loop
            Prepare_Semres
              (UFP.Get_Unit (Ctx, Text_IO & "." & SP.all, Kind));
         end loop;
      end if;

      --  In Ada, "with A.B" gives visibility to A and A.B. To process all
      --  "mentioned" units, the following loop iterates on ["A.B", "A"].

      for I in reverse Name'Range loop
         declare
            Current_Name : Symbol_Type_Array := Name (Name'First .. I);
         begin
            --  TODO??? Find a proper way to handle file not found, parsing
            --  error, etc.
            Unit := UFP.Get_Unit (Ctx, To_String (Current_Name), Kind);
            Prepare_Semres (Unit);

            --  The first iteration gives the unit we are required to return
            if First_Unit = No_Analysis_Unit then
               First_Unit := Unit;
            end if;
         end;
      end loop;
      return First_Unit;
   end Fetch_Unit;

end Libadalang.Unit_Files;
