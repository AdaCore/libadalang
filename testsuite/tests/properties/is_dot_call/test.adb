procedure Test is
   package T is
      type TT is tagged null record;
      procedure Foo (Self : TT) is null;
   end T;

   Inst : T.TT;
begin
   Inst.Foo; -- Dot call
   T.Foo (Inst); -- Not dot call
end Test;
