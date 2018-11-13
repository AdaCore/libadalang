procedure Test is
   package T is
      type TT is tagged null record;
      procedure Foo (Self : TT; X : Integer) is null;
   end T;

   Inst : T.TT;
begin
   Inst.Foo (Pouet); -- Dot call
   T.Foo (Inst, 2); -- Not dot call
end Test;
