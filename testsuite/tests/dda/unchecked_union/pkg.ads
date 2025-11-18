--  Check the handling of unchecked unions

package Pkg is

   type Rec_Type (N : Integer) is record
      case N is
         when Integer'First .. -1 =>
            XB : Boolean;

         when 0 =>
            XC : Character;

         when others =>
            XI : Integer;
      end case;
   end record
   with Unchecked_Union;

end Pkg;
