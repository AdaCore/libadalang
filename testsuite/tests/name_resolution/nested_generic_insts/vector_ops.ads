with Vector;

generic
   type T is private;
package Vector_Ops is
   package Internal_Vector is new Vector (Integer);

   generic
      with function Predicate (El : T) return Boolean;
   function Filter (V : Internal_Vector.Vector) return Internal_Vector.Vector;
end Vector_Ops;
