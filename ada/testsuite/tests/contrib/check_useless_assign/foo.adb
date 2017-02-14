procedure Foo is

   type Rec is record
      X, Y : Integer;
   end record;

   R : Rec;
   V : Integer;

   procedure Check (W : Integer) is
   begin
      if V /= W then
        return;
      end if;
   end Check;

begin
   R := (0,0);
   R.X := 0;
   R.X := 1;

   if R.X = 1 then
      R.X := 0;
   end if;
   R.X := 1;

   if R.X = 1 then
      R.X := 0;
   else
      R.Y := R.X;
   end if;
   R.X := 1;

   if R.X = 1 then
      R.X := 0;
   else
      R.X := R.Y;
   end if;
   R.X := 1;
   R.X := R.X + 1;

   if R.X = 1 then
      V := 0;
   end if;

   V := V + 1;

   Check(1);
   V := 0;

   while True loop
      if R.X = 1 then
         R.X := 2;
      end if;
   end loop;

   declare
      Var : Integer;
      pragma Warnings (Off, Var);
   begin
      Var := 1;
   end;

   if True then
      V := 1;
   else
      V := 0;
   end if;
   Check (3);

   case V is
      when 0 =>
         V := 0;
      when others =>
         V := 1;
   end case;

end Foo;
