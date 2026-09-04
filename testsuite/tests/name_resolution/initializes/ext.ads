package Ext
  with Abstract_State => State, Initializes => (State, V, W)
is
   V : Integer;
   W : Integer;
end Ext;
pragma Test_Block;
