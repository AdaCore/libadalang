procedure Aspec is
   function Foo (X : Natural; Y : Natural with Unreferenced)
      return Natural is (2 * X);
   --% node.find(lal.ParamSpecList)[1].p_has_aspect('Fail')
   --% node.find(lal.ParamSpecList)[1].p_has_aspect('Unreferenced')
   --% node.find(lal.ParamSpecList)[1].p_get_aspect('Fail')
   --% node.find(lal.ParamSpecList)[1].p_get_aspect('Unreferenced')
begin
   null;
end Aspec;
