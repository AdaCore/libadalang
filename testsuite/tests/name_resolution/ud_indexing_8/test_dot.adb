procedure Test_Dot is
   package Pkg is
      --  Same incompatible-type design as test.adb: CI returns an integer-range
      --  type and VI dereferences to a Boolean-derived type, ensuring each
      --  test case has a single unambiguous resolution.
      type CI_Result is range 1 .. 100;

      type Mutable_T is new Boolean;
      type VI_Result (R : access Mutable_T) is null record
      with Implicit_Dereference => R;

      Target : aliased Mutable_T := True;

      type Container is tagged null record
      with Constant_Indexing => Get_Const, Variable_Indexing => Get_Var;

      function Get_Const (C : Container; I : Integer) return CI_Result
      is (42);
      function Get_Var (C : Container; I : Integer) return VI_Result
      is ((R => Target'Access));

      --  A tagged receiver type whose primitives can be called with dot
      --  notation.  The Self parameter is the implicit first formal, so the
      --  ud-indexed actuals map to the second formal and beyond.
      type Receiver is tagged null record;

      procedure Dot_Take_In (Self : Receiver; X : CI_Result) is null;
      procedure Dot_Take_Out (Self : Receiver; X : out VI_Result) is null;
      procedure Dot_Take_In_Out (Self : Receiver; X : in out VI_Result)
      is null;
      procedure Dot_Take_Mixed
        (Self : Receiver; X : CI_Result; Y : out VI_Result)
      is null;
   end Pkg;

   use Pkg;

   C   : Container := (null record);          --  variable object
   C_K : constant Container := (null record); --  constant object
   Rec : Receiver := (null record);

begin
   --  in param, dot notation, positional: X is in -> constant indexing
   Rec.Dot_Take_In (C (1));
   pragma Test_Statement;  --  C (1) -> Get_Const

   --  out param, dot notation, positional: X is out -> variable indexing
   Rec.Dot_Take_Out (C (1));
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  in out param, dot notation, positional: X is in out -> variable indexing
   Rec.Dot_Take_In_Out (C (1));
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  Constant object: case (2) forces constant indexing regardless of mode
   Rec.Dot_Take_In (C_K (1));
   pragma Test_Statement;  --  C_K (1) -> Get_Const

   --  Named in param, dot notation -> constant indexing
   Rec.Dot_Take_In (X => C (1));
   pragma Test_Statement;  --  C (1) -> Get_Const

   --  Named out param, dot notation -> variable indexing
   Rec.Dot_Take_Out (X => C (1));
   pragma Test_Statement;  --  C (1) -> Get_Var

   --  Mixed positional, dot notation: X (in) -> CI, Y (out) -> VI
   Rec.Dot_Take_Mixed (C (1), C (1));
   pragma Test_Statement;  --  C (1) at X -> Get_Const, C (1) at Y -> Get_Var

   --  Mixed reversed named, dot notation: same result regardless of order
   Rec.Dot_Take_Mixed (Y => C (1), X => C (1));
   pragma Test_Statement;  --  C (1) at X -> Get_Const, C (1) at Y -> Get_Var

end Test_Dot;
