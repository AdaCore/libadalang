package Pkg is

   generic
   package Gen is
      I : Integer;
   end Gen;

   package Inst is new Gen;

   I : Integer := Inst.I;

end Pkg;
