package Foo is

   generic
      type T is private;
   package Opt_Types is
      type Opt_Type (Present : Boolean) is record
         case Present is
            when True  => Value : T;
            when False => null;
         end case;
      end record;
   end Opt_Types;

   generic
      with package Opt is new Opt_Types (<>);
   function Extract (V : Opt.Opt_Type) return Opt.T;

end Foo;
