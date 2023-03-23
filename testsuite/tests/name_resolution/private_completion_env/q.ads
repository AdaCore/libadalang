package Q is
   type T is private;

   X : constant T;
private
   type T is record
      Val : Integer := 0;
   end record;

   X : constant T := (Val => 1);
end Q;
