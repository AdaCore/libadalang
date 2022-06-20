generic
   type Element is (<>);
package Main_Generic_Package is

   pragma Pure;

   generic
      with function Element_Op (X, Y : Element) return Element;
   procedure Operation (X : Integer);

end Main_Generic_Package;
