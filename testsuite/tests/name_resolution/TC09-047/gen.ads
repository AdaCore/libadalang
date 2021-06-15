generic
   type Base_Type is tagged private;
package Gen is

   type Counted_Type is new Base_Type with record
      Count : Natural := 0;
   end record;

   function Count (Obj : Counted_Type) return Natural is (Obj.Count);
   -- Return the count of Obj.

end Gen;
