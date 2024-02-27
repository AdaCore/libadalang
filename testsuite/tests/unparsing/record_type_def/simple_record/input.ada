type Date is record
      Day   : Integer range 1 .. 31;
      Month : Month_Enum;
      Year  : Integer range 0 .. 4000;
   end record;