procedure Proc is
   type Primary_Color is (Red, Green, Blue);

   type PC1 is new Primary_Color;
   type PC2 is new Primary_Color;
   type PC3 is new Primary_Color;

   for PC1 use (-3, 8, 456);
   --% node.p_params
   for PC2 use (Red => -3, Green => 8, Blue => 456);
   --% node.p_params
   for PC3 use (Green => 8, Red => -3, Blue => 456);
   --% node.p_params

   type My_Bool3 is new Boolean;
   for My_Bool3 use (True => 2, False => 0);
   --% node.p_params

   type My_Bool4 is new My_Bool3;
   for My_Bool4 use (0, 1);
   --% node.p_params
begin
   null;
end Proc;
