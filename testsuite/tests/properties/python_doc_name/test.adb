package Pkg is
   type My_String is new String;

   package Child_Pkg is
      type My_Int is new Integer;
   end Child_Pkg;

   use Child_Pkg;

   procedure Foo (I : My_Int);
   --% $node.findall(lal.Name)[-1].doc_name
end Pkg;

with Pkg; use Pkg;
package Pkg2 is
   type My_Other_String is new String;

   procedure Foo (S : Pkg.My_String);
   --% $node.findall(lal.Name)[-1].doc_name

   procedure Bar (S : My_String);
   --% $node.findall(lal.Name)[-1].doc_name

   procedure Baz (S : Pkg2.My_Other_String);
   --% $node.findall(lal.Name)[-1].doc_name
end Pkg2;
