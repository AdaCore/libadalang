procedure Foo is

   type Ptr is access Integer;
   P : Ptr;
   X : Integer;

   type Rec is record
      D : Integer;
   end record;
   type Rec_Ptr is access Rec;
   R : Rec_Ptr;

begin
   X := P.all;
   if P = null then
      return;
   end if;

   if True then
      X := R.D;
   end if;
   if R = null then
      return;
   end if;

end Foo;
