procedure Gen_Indexing is
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
   --% node.f_dest.p_call_params
   I := Fn (3) (2);
   --% node.f_expr.p_call_params
   I := A (2) (3);
   --% node.f_expr.p_call_params
end Gen_Indexing;


