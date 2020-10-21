package Pack is

   type Some_Task;

   type Some_Task is limited private;

private

   package T is
      procedure Foo;
   end T;


   task type Some_Task is
     entry Start;
   end Some_Task;

end Pack;
