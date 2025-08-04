pragma Ada_2022;

procedure Test is
   type E is (X, Y, Z);

   package A is
      type Root is tagged record X : Integer; end record;
      function Make return Root is (X => 0);
      function Make2 return Root is (X => 1);
   end A;
   use A;
   package B is
      type Child is new Root with null record;
      overriding function Make return Child is (X => -1);
      overriding function Make2 return Child is (X => -2);
   end B;
   function Test return Boolean is (True);
   U : Root'Class := B.Child'(X => 3);
   V : Root'Class := Make2;

   O : E := X;
begin
   --  All calls to Make/Make2 below are dispatching

   V := (Make);

   U := (if Test then Make elsif Test then Make else Make2);

   U := (case O is when X => Make, when Y => Make, when others => Make2);

   U := (declare I : constant Integer := 1; begin Make);

   U := (
      declare I : constant Integer := 1;
      begin
         (if Test
          then Make
          elsif Test
          then (((Make)))
          else (case O is
                   when X => Make,
                   when Y => Make,
                   when others => Make2))
   );
end Test;
