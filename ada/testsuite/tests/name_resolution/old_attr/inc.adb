procedure Increment (X : in out Integer) with
  Post => X = X'Old + 1;
pragma Test_Block;
