package Pkg is
   type T is null record;

   generic
   package Nested is
      procedure Proc;
   end Nested;

   procedure Stub;
end Pkg;
