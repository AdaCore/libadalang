procedure Geni is
   generic
      type T is private;
   procedure P1 (A : T);

   generic
      type T1 is private;
      type T2 is private;
   procedure P2 (A : T1; B : T1; C : T2);

   generic
      type T is private;
      X, Y : T;
   procedure P4 (A : T);

   generic
      type T is private;
      X, Y : T;
      M : Integer := 0;
      N : Integer := 0;
   procedure P5 (A : T);

   generic
      type T is private;
      X, Y: T;
      M, N : Integer := 0;
   procedure P6 (A : T);

   procedure P1 (A : T) is null;
   procedure P2 (A : T1; B : T1; C : T2) is null;
   procedure P4 (A : T) is null;
   procedure P5 (A : T) is null;
   procedure P6 (A : T) is null;

   procedure I1P1 is new P1 (Integer);
   --% node.p_inst_params
   procedure I2P1 is new P1 (T => Integer);
   --% node.p_inst_params

   procedure I1P2 is new P2 (Integer, Float);
   --% node.p_inst_params
   procedure I2P2 is new P2 (T2 => Integer, T1 => Float);
   --% node.p_inst_params

   procedure I1P4 is new P4 (Integer, 0, 1);
   --% node.p_inst_params
   procedure I2P4 is new P4 (X => 0, Y => 0, T => Integer);
   --% node.p_inst_params
   procedure I3P4 is new P4 (Integer, Y => 0, X => 0);
   --% node.p_inst_params

   procedure I1P5 is new P5 (Integer, 0, 1, 2, 3);
   --% node.p_inst_params
   procedure I2P5 is new P5 (Integer, 0, 1, N => 2, M => 3);
   --% node.p_inst_params
   procedure I3P5 is new P5 (Integer, Y => 0, M => 1, N => 2, X => 3);
   --% node.p_inst_params
   procedure I4P5 is new P5 (Y => 0, M => 1, N => 2, X => 3, T => Integer);
   --% node.p_inst_params
   procedure I5P5 is new P5 (Integer, Y => 0, X => 3);
   --% node.p_inst_params
   procedure I6P5 is new P5 (Integer, Y => 0, M => 0, X => 3);
   --% node.p_inst_params

   procedure I1P6 is new P6 (Integer, 0, 1, 2, 3);
   --% node.p_inst_params
   procedure I2P6 is new P6 (Integer, 0, 1, N => 2, M => 3);
   --% node.p_inst_params
   procedure I3P6 is new P6 (Integer, Y => 0, M => 1, N => 2, X => 3);
   --% node.p_inst_params
   procedure I4P6 is new P6 (Y => 0, M => 1, N => 2, X => 3, T => Integer);
   --% node.p_inst_params
   procedure I5P6 is new P6 (Integer, Y => 0, X => 3);
   --% node.p_inst_params
   procedure I6P6 is new P6 (Integer, Y => 0, M => 0, X => 3);
   --% node.p_inst_params

   generic
      type T is private;
      X : Integer := 0;
      Y, Z : Integer := 1;
      M : Integer;
   package PA is
      procedure P1 (A : T);
   private
      procedure P1 (A : T) is null;
   end PA;

   package I1PA is new PA (Integer, M => 0);
   --% node.p_inst_params
   package I2PA is new PA (T => Integer, M => 8);
   --% node.p_inst_params
   package I3PA is new PA (Integer, 0, 0, 0, 0);
   --% node.p_inst_params
   package I4PA is new PA (Integer, Z => 0, X => 0, Y => 0, M => 0);
   --% node.p_inst_params
   package I5PA is new PA (X => 0, Y => 0, Z => 0, T => Integer, M => 0);
   --% node.p_inst_params

begin
   null;
end Geni;
