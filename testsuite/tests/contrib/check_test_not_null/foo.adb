procedure Foo is

   type Ptr is access Integer;
   type Rec is record
      C : Ptr;
      D : Integer;
   end record;
   type Rec_Ptr is access all Rec;

   R : aliased Rec;
   P : Rec_Ptr := R'Access;
   X : Integer;
   T : A.B.C; -- type A.B.C does not exist

begin
   T.x.all := 0;
   T := null;
   T.x.all := 0;

   if R.C = null then
      R.C.all := 0;
      return;
   elsif R.C = null and then R.C.all = 0 then
      return;
   elsif R.C /= null or else R.C.all = 0 then
      return;
   end if;

   if P.C = null then
      if True then
         X := P.C.all;
      end if;
      return;
   else
      P.C := null;
      X := P.C.all;
   end if;

   pragma Assert (P.C = null);
   X := P.C.all;
end Foo;
