generic
   type T1 is private;
   type T2 is private;
package Tuple is
   type Tuple is record
      Field_1 : T1;
      Field_2 : T2;
   end record;
end Tuple;
