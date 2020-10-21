package body Pack is

   package body T is
      procedure Foo is null;
   end T;

   task body Some_Task is
   begin
     select
     accept Start do null; end Start;
     end select;
   end Some_Task;

end Pack;
