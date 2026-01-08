procedure Test is
   package P is
      type My_Container is tagged record
         X : aliased Integer;
      end record with Constant_Indexing => Find;

      type Ref (Data : not null access Integer) is null record
        with Implicit_Dereference => Data;

      function Find (Self : in My_Container; X : String) return Ref
        with Import;
      function Find (Self : in My_Container; X : Integer) return Ref
        with Import;

      function Length (Self : in My_Container) return Integer with Import;
   end P;

   use P;

   type Arr is array (1 .. 10) of My_Container;

   type Get_Cont is access function (X : Integer) return My_Container;

   R  : My_Container;
   Fn : Get_Cont;
   A  : Arr;
   I  : Integer;
begin
   R ("hello") := 42;
   --% node.f_dest.p_is_prefix_call
   I := Fn (3) (2);
   --% node.f_expr.p_is_prefix_call
   --% not node.f_expr.f_name.p_is_prefix_call
   A (2) := R;
   --% not node.f_dest.p_is_prefix_call
   I := A (2) (3);
   --% node.f_expr.p_is_prefix_call
   --% not node.f_expr.f_name.p_is_prefix_call
   I := A (2).Length;
   --% node.f_expr.p_is_prefix_call
   I := Length (R);
   --% not node.f_expr.p_is_prefix_call
   I := A (2).X;
   --% not node.f_expr.p_is_prefix_call
   S : String := A (2).X'Image;
   --% node.f_default_expr.p_is_prefix_call
   S_2 : String := Integer'Image (A (2).X);
   --% not node.f_default_expr.p_is_prefix_call
end;

