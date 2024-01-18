--  All these tests have been derived from the SPARK documentation

procedure Test is
   X, Y, Z, A, B, C, D, G : Integer;

   procedure P (X, Y, Z : in Integer; Result : out Boolean)
     with Depends => (Result => (X, Y, Z));
   pragma Test_Block;

   procedure Q (X, Y, Z : in Integer; A, B, C, D, E : out Integer)
     with Depends => ((A, B) => (X, Y),
                      C      => (X, Z),
                      D      => null,
                      E      => Y);
   pragma Test_Block;

   procedure R (X, Y, Z : in Integer; A, B, C, D : in out Integer)
     with Depends => ((A, B) =>+ (A, X, Y),
                      C      =>+ Z,
                      D      =>+ null);
   pragma Test_Block;

   procedure S
     with Global  => (Input  => (X, Y, Z),
                      In_Out => (A, B, C, D)),
          Depends => ((A, B) =>+ (A, X, Y, Z),
                      C      =>+ Y,
                      D      =>+ null);
   pragma Test_Block;

   function F (X, Y : Integer) return Integer
     with Global  => G,
          Depends => (F'Result => (G, X),
                      null     => Y);
   pragma Test_Block;

   procedure P (X, Y, Z : in Integer; Result : out Boolean) is
   begin
      null;
   end;

   procedure Q (X, Y, Z : in Integer; A, B, C, D, E : out Integer) is
   begin
      null;
   end;

   procedure R (X, Y, Z : in Integer; A, B, C, D : in out Integer) is
   begin
      null;
   end;

   procedure S is
   begin
      null;
   end;

   function F (X, Y : Integer) return Integer is
   begin
      return 0;
   end;
begin
   null;
end Test;
