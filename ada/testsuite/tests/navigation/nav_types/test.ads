package Test is
   type A;

   type A is private;

private
   type A is record
      C, D : Integer;
   end record;
end Test;
