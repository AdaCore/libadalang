with Pkg; use Pkg;
package Pkg2 is
   type My_Other_String is new String;

   procedure Foo (S : Pkg.My_String);
   --% node.findall(lal.Name)[-1].doc_name

   procedure Bar (S : My_String);
   --% node.findall(lal.Name)[-1].doc_name

   procedure Baz (S : Pkg2.My_Other_String);
   --% node.findall(lal.Name)[-1].doc_name
end Pkg2;
