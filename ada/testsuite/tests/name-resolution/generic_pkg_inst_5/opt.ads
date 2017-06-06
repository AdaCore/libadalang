generic
   type T is private;
package Opt is

   type Opt_Type (B : Boolean := False) is record
      case B is
         when True => Value : T;
         when False => null;
      end case;
   end record;

   function Create return Opt_Type;

end Opt;
