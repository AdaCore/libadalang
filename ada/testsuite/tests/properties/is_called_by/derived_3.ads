with Derived_2;

package Derived_3 is
   type T is new Derived_2.T with null record;

   overriding procedure Bar (Self : T) is null;
end Derived_3;

