package body Pkg is
   type T is record
      Foo : Integer;
   end record;

   function Test (X : T_Access) return Integer is
   begin
      return X.all.Foo;
   end Test;
end Pkg;
