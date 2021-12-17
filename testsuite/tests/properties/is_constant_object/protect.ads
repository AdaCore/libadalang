package Protect is
   protected type T is
      procedure Proc;
      function Func return Integer;

      function Foo (A : in out Integer) return Integer;
   private
      X : Integer;
   end T;

   PO : T;
   --% node.p_is_constant_object
end Protect;
