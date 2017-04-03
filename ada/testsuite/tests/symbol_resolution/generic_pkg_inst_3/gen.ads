package Gen is

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

end Gen;
