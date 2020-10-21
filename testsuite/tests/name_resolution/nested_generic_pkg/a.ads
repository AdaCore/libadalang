generic
type T is private;
package A is
   generic
      type U is private;
   package B is
      function Convert (S : T) return U;
   end B;
end A;
