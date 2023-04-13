generic
   type Item_T is private;
   First_Item : Item_T;
package GP is
   type T is private;

   function First (Interval : T) return Item_T;
private
   type T is record
      First : Item_T;
   end record;

   function First (Interval : T) return Item_T is (Interval.First);
end GP;
