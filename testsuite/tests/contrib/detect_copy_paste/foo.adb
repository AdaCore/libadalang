procedure Foo is

   procedure Dummy1 (X : in out Integer) is
      Y : Integer;
   begin
      Y := X + X;
      X := Y * Y;
      if X < 10 then
         X := Y;
      else
         Y := X;
      end if;
   end Dummy1;

   procedure Dummy2 (X : in out Integer) is
      Y : Integer;
   begin
      Y := X + X;
      X := Y * Y;
      if X < 10 then
         X := Y;
      else
         Y := X;
      end if;
   end Dummy2;

   X : Integer := 1;
begin
   Dummy1 (X);
   Dummy2 (X);
end Foo;
