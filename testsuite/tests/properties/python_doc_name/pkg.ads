package Pkg is
   type My_String is new String;

   package Child_Pkg is
      type My_Int is new Integer;
   end Child_Pkg;

   use Child_Pkg;

   procedure Foo (I : My_Int);
   --% node.findall(lal.Name)[-1].doc_name
end Pkg;
