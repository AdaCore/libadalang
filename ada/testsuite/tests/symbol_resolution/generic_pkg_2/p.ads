generic
   type T is private;
   type U is private;
   with function Convert (Self : T) return U;
package P is
   procedure Foo (Self : T);
end P;
