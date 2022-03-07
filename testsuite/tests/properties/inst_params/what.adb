procedure What is
   procedure Foo is null;

   generic
      type T is private;
   package Boo is
      procedure Baa (Self : T) is null;
   end Boo;

   generic
      type T is private;
      with package Bla is new Boo (T);
      with procedure Whatever (Self : T) is Bla.Baa;
   package Wat is
   end Wat;

   package Inst_1 is new Boo (Integer);
   --% node.p_inst_params
   package Inst_2 is new Wat (Integer, Inst_1);
   --% node.p_inst_params
begin
   null;
end What;
