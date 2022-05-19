package Foo is
   type T is (A, B, C);
   subtype ST is T range B .. C;
   --% node.p_get_type()

end Foo;
