with C;

package D is
   type Foo is null record;

   procedure Pouet (Self : in out Foo; R : C.Root_Type);
end D;
