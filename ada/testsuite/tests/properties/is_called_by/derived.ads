with Base;

package Derived is
   type T is new Base.T with null record;

   overriding procedure Bar (Self : T) is null;
end Derived;
