task type Keyboard_Driver (ID : Keyboard_ID := New_ID) is new Serial_Device with
   entry Read (C : out Character);
   entry Write(C : in  Character);
end Keyboard_Driver;
