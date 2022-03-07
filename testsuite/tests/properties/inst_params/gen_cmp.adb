procedure Gen_Cmp is

   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">" (Left, Right : Element_Type) return Boolean is <>;
   procedure Gen (Data : in out Array_Type);

   procedure Gen (Data : in out Array_Type) is
   begin
      null;
   end;

   type Alpha is
     (A, B, C, D, E, F, G, H, I, J, K, L, M,
      N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

   type My_Array is array (Alpha range <>) of Integer;

   function Less_Than (L, R : Integer) return Boolean is
   begin
      return L < R;
   end;

   procedure Flag1 is new Gen (Element_Type => Integer,
                               Index_Type   => Alpha,
                               Array_Type   => My_Array,
                               ">"          => Less_Than);
   --% node.p_inst_params

   procedure Flag2 is new Gen (Integer, Alpha, My_Array, Less_Than);
   --% node.p_inst_params

begin
   null;
end;
