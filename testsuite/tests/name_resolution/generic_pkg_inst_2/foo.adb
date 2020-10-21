package body Foo is

   function Extract (V : Opt.Opt_Type) return Opt.T is
   begin
      if V.Present then
         return V.Value;
      end if;
      raise Constraint_Error;
   end Extract;

end Foo;
