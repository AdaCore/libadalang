procedure Test is
   type R is record
      X : Boolean;
   end record;

   function Foo (X : Integer) return Integer is (X)
      with Pre => Bar (X).X --% node.p_expression_type
      ;

   function Bar (X : Integer) return R is (X => True);
begin
   null;
end Test;
