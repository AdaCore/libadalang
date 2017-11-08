with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Unit_Files.Default; use Libadalang.Unit_Files.Default;

package body Libadalang.Unit_Files is

   Text_IO        : constant String := "ada.text_io";
   Integer_IO     : aliased constant String := "integer_io";
   Float_IO       : aliased constant String := "float_io";
   Fixed_IO       : aliased constant String := "fixed_io";
   Decimal_IO     : aliased constant String := "decimal_io";
   Enumeration_IO : aliased constant String := "enumeration_io";
   Text_IO_Subpackages : constant array (Positive range <>)
                                  of access constant String :=
     (Integer_IO'Access, Float_IO'Access, Fixed_IO'Access, Decimal_IO'Access,
      Enumeration_IO'Access);

   ----------------
   -- Fetch_Unit --
   ----------------

   function Fetch_Unit
     (Ctx  : Analysis_Context;
      Name : Bare_Ada_Node;
      Kind : Unit_Kind) return Analysis_Unit
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
            Reference_Unit (From       => Get_Unit (Create (Name)),
                            Referenced => Unit);
         end if;
      end Prepare_Semres;

      UFP              : constant Unit_Provider_Access_Cst :=
         Unit_Provider (Ctx);

      Unit, First_Unit : Analysis_Unit;
      Current_Name     : Bare_Ada_Node := Name;

   begin

      --  In Ada, "with A.B" gives visibility to A and A.B. To process all
      --  "mentionned" units, the following loop iterates on ["A.B", "A"].

      while Current_Name /= null loop
         --  TODO??? Find a proper way to handle file not found, parsing error,
         --  etc.
         Unit := UFP.Get_Unit
           (Ctx, Unit_Text_Name (Bare_Name (Current_Name)), Kind);
         Prepare_Semres (Unit);

         --  GNAT kludge: as an "optimization", the generic subpackages in
         --  Ada.Text_IO (see Text_IO_Subpackages) are not present in the
         --  Ada.Text_IO unit itself, but in private child packages. GNAT
         --  magically imports them in Ada.Text_IO's namespace.
         --
         --  Here, try to import these child unit as soon as someone WITHes
         --  Ada.Text_IO.

         if Kind = Unit_Specification
            and then Unit_String_Name (Bare_Name (Name)) = Text_IO
         then
            for SP of Text_IO_Subpackages loop
               declare
                  SU : constant Analysis_Unit := UFP.Get_Unit
                    (Ctx, To_Text (Text_IO & "." & SP.all), Kind);
               begin
                  Prepare_Semres (SU);
               end;
            end loop;
         end if;

         --  The first iteration gives the unit we are required to return
         if First_Unit = No_Analysis_Unit then
            First_Unit := Unit;
         end if;

         --  Fetch the next mention name to process
         if Current_Name.all in Bare_Base_Id_Type'Class then
            Current_Name := null;
         elsif Current_Name.all in Bare_Dotted_Name_Type'Class then
            Current_Name :=
               Bare_Ada_Node (Bare_Dotted_Name (Current_Name).F_Prefix);
         else
            raise Property_Error;
         end if;
      end loop;

      return First_Unit;
   end Fetch_Unit;

end Libadalang.Unit_Files;
