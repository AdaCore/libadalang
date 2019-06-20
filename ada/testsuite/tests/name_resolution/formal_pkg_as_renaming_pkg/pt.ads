with Data_Pkg;

generic
   with package Pkg is new Data_Pkg (<>);
package PT is
   procedure Get (Value : Pkg.Value_T) is null;
end PT;
