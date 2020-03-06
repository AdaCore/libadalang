with G;

generic
   with package H is new G (<>);
package P is
   function Foo (X : H.T) return Integer is (42);
end P;
