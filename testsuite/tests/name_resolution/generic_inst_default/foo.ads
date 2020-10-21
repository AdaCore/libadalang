with Bar;

generic
   with package P is new Bar (<>);
package Foo is
   procedure Proc (E : P.Element_Type) renames P.Proc;
end Foo;
