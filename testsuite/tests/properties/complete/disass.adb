procedure Disass is

   type Device is (Printer, Disk, Drum);
   type State  is (PriOpen, Closed);

   type Peripheral(Unit : Device := Disk) is
      record
         Status : State;
         case Unit is
            when Printer =>
               Line_Count : Integer range 1 .. Page_Size;
            when others =>
               Cylinder   : Cylinder_Index;
               Track      : Track_Number;
         end case;
      end record;

   subtype Drum_Unit is Peripheral (   );
   --% list(node.f_subtype.f_constraint.p_complete)

   Writer : Peripheral (Unit =>    );
   --% list(node.f_type_expr.f_constraint.p_complete)
