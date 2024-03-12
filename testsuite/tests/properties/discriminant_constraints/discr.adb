procedure Discr is
   type Device is (Printer, Disk, Drum);
   type State  is (Open, Closed);
   subtype Cylinder_Index is Integer range 1 .. 300;
   subtype Track_Number is Integer range 1 .. 300;

   type Peripheral(Unit : Device := Disk; Trk1, Trk2 : Track_Number := 5) is
      record
         Status : State;
         case Unit is
            when Printer =>
               Line_Count : Integer range 1 .. 10 := 4;
            when others =>
               Cylinder   : Cylinder_Index;
               Track1     : Track_Number := Trk1;
               --% node.p_type_expression.p_discriminant_constraints
               Track2     : Track_Number := Trk2;
         end case;
      end record;

   P1 : Peripheral;
   --% node.f_type_expr.p_discriminant_constraints
   P2 : Peripheral (Unit => Printer, Trk1 => 2, Trk2 => 1);
   --% node.f_type_expr.p_discriminant_constraints
   P3 : Peripheral (Printer, 3, 4);
   --% node.f_type_expr.p_discriminant_constraints
   P4 : Peripheral (Trk1|Trk2 => 2, Unit => Printer);
   --% node.f_type_expr.p_discriminant_constraints

begin
   null;
end Discr;
