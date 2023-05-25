with Types;

package Objects is
   O1 : Types.T1;
   pragma Test_Statement;
end Objects;

package Types is
   type T1 is new Integer;
   pragma Test_Statement;
end Types;
