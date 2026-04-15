function Foo_Ret (SCOD : Bar) return Boolean is
begin
   return
     --  the empty line after this comment triggers bug

     SCOD.Origin = False;
end Foo_Ret;
