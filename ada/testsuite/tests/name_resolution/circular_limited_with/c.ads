limited with D;

package C is
   type Root_Type is null record;

   procedure Visit (R : Root_Type; X : in out D.Foo) is null;
end C;
