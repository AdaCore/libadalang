procedure Test is
   package P is
      type My_Container is tagged record
         X : aliased Integer;
      end record with Variable_Indexing => Find;

      type Ref (Data : not null access Integer) is null record
        with Implicit_Dereference => Data;

      function Find (Self : in out My_Container; X : String) return Ref
        with Import;
      function Find (Self : in out My_Container; X : Integer) return Ref
        with Import;
   end P;

   use P;

   R : My_Container;
begin
   R ("hello") := 42;
   --% node.f_dest.p_called_subp_spec
   R (1) := 42;
   --% node.f_dest.p_called_subp_spec
end;

