pragma Ada_2012;

-- In this instance, we exercise variable user indexing
-- yielding Integer values.

package FUAND is

   type Op_Name is (Op_A, Op_B);

   type Operands is tagged record
      A, B : aliased Integer;
   end record
     with Variable_Indexing => V_Indexing;

   type Integer_Ref (Ref : access Integer) is
     null record with Implicit_Dereference => Ref;

   function V_Indexing
     (X : aliased in out Operands; Op : Op_Name)
     return integer_Ref is
      (Ref =>
         (case Op is
            when OP_A => X.A'Access,
            when Op_B => X.B'Access));


   function Andthen (Ops : Operands) return Boolean;
end;
