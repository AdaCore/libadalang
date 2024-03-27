package Test.Child is
   --  SPARK_Mode => On, because Test.Child is On because of the default in the
   --  .adc but doesn't inherit value from parent library package.
   procedure P;
end Test.Child;
