with Ext;

package Test_Pragma
  with Abstract_State => (S1, S2)
is
   pragma
     Initializes ((S1, Y, S2 => Ext.State, X => (Ext.V, Ext.W), Z => null));
   pragma Test_Statement;

   X : Integer;
   Y : Integer;
   Z : Integer;

   package Single is
      pragma Initializes (Var);
      pragma Test_Statement;

      Var : Integer := 0;
   end Single;

   package Nothing is
      pragma Initializes (null);
      pragma Test_Statement;
   end Nothing;
end Test_Pragma;
