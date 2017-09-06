with Vectors;

generic
   type Value_Type is private;
package Options is

   type T (Present : Boolean := False) is record
      case Present is
         when True  => Value : Value_Type;
         when False => null;
      end case;
   end record;

   package Option_Vectors is new Vectors (T);

end Options;
