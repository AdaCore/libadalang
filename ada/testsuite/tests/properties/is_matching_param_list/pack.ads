package Pack is

   function F1 return Integer;

   function F2 (I : Integer) return Integer;

   function F3 (I, J : Integer) return Integer;

   function F4 (I : Integer; J : Integer := 0) return Integer;

   V1_Fail_1 : Integer := F1 (1);
   V1_Fail_2 : Integer := F1 (I => 1);

   V2_OK_1   : Integer := F2 (1);
   V2_OK_2   : Integer := F2 (I => 1);
   V2_Fail_3 : Integer := F2 (1, 2);
   V2_Fail_4 : Integer := F2 (J => 1);
   V2_Fail_5 : Integer := F2 (1, I => 2);
   V2_Fail_6 : Integer := F2 (J => 1);

   V3_OK_1   : Integer := F3 (1, 2);
   V3_OK_2   : Integer := F3 (1, J => 2);
   V3_OK_3   : Integer := F3 (J => 1, I => 2);
   V3_Fail_1 : Integer := F3 (1);
   V3_Fail_2 : Integer := F3 (1, 2, 3);
   V3_Fail_3 : Integer := F3 (I => 1);
   V3_Fail_4 : Integer := F3 (J => 1);
   V3_Fail_5 : Integer := F3 (1, I => 2);
   V3_Fail_6 : Integer := F3 (1, I => 1, J => 2);

   V4_OK_1   : Integer := F4 (1, 2);
   V4_OK_2   : Integer := F4 (1);
   V4_OK_3   : Integer := F4 (I => 1);
   V4_OK_4   : Integer := F4 (I => 1, J => 2);
   V4_OK_5   : Integer := F4 (J => 1, I => 2);
   V4_Fail_1 : Integer := F4 (J => 1);
   V4_Fail_2 : Integer := F4 (1, J => 1, I => 2);

end Pack;
