procedure Test is
   function F (F : Boolean) return Integer is (0);
   function F (I : Integer) return Integer is (0);

   generic
      type T is (<>);
      with function F (X : T) return Integer is <>;
   procedure Gen;

   procedure Gen is
   begin
      null;
   end Gen;

   procedure Inst1 is new Gen (Boolean);
   procedure Inst2 is new Gen (Integer);
begin
   null;
end;
