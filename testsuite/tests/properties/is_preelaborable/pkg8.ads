generic
   type T is private;
package Pkg8 is
   pragma Preelaborate;

   function Ident (X : T) return T;
end Pkg8;
