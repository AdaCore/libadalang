--  Check the handling of subtypes for records with variant parts

package Pkg is

   --  Discriminated record type with variant parts

   type Rec_Type (N : Integer) is record
      case N is
         when Integer'First .. -1 =>
            XB : Boolean;

         when 0 =>
            XC : Character;

         when others =>
            XI : Integer;
      end case;
   end record;

   --  Constraint with a compile-time known value

   subtype S_Cons_0 is Rec_Type (0);

   --  Indirect constraint with a compile-time known value

   subtype S_Indirect_0 is Rec_Type;
   subtype S_Indirect_1 is S_Indirect_0 (1);
   subtype S_Indirect_2 is S_Indirect_1;
   subtype S_Indirect is S_Indirect_2;

   --  Derived type with constraint

   type D_Direct is new Rec_Type (-1);

   --  Constraint through a dynamic predicate

   subtype S_Pred_Nat is Rec_Type
   with Dynamic_Predicate => S_Pred_Nat.N in Natural;

   --  Constraint with a value not known at compile time

   function Get_Integer return Integer with Import;
   subtype S_Cons_Dyn is Rec_Type (Get_Integer);

end Pkg;
