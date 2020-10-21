generic
   type Counter is private;
package Bar is

   generic
      Init : Counter;
   package Gen is
      Count : Counter := Counter (Init);
   end;

end Bar;
