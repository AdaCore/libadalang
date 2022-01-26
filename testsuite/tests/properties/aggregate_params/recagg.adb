procedure RecAgg is
   type Month_Name is (January, February, March, April, May, June, July,
                    August, September, October, November, December);

   type Date is
      record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;

   type Device is (Printer, Disk, Drum);
   type State  is (Open, Closed);
   subtype Cylinder_Index is Integer range 1 .. 300;
   subtype Track_Number is Integer range 1 .. 300;

   type Peripheral(Unit : Device := Disk) is
      record
         Status : State;
         case Unit is
            when Printer =>
               Line_Count : Integer range 1 .. 10 := 4;
            when others =>
               Cylinder   : Cylinder_Index;
               Track      : Track_Number := 10;
         end case;
      end record;

   type Cell;
   type Link is access Cell;

   type Cell is
      record
         Value  : Integer;
         Succ   : Link;
         Pred   : Link;
      end record;

   type Rec is tagged
      record
         A : Integer;
      end record;

   type ExtR is new Rec with
      record
         B : Integer;
      end record;

   D : Date;
   P : Peripheral;
   C : Cell;
   R : Rec;
   E : ExtR;

begin
   D := (4, July, 1776);
   --% node.f_expr.p_aggregate_params
   D := (Day => 4, Month => July, Year => 1776);
   --% node.f_expr.p_aggregate_params
   D := (Month => July, Day => 4, Year => 1776);
   --% node.f_expr.p_aggregate_params

   P := (Disk, Closed, Track => 5, Cylinder => 12);
   --% node.f_expr.p_aggregate_params
   P := (Unit => Disk, Status => Closed, Cylinder => 9, Track => 1);
   --% node.f_expr.p_aggregate_params
   P := (Disk, Status => Closed, Cylinder => 12, Track => 2);
   --% node.f_expr.p_aggregate_params
   P := (Disk, Closed, 9, 1);
   --% node.f_expr.p_aggregate_params
   P := (Disk, Closed, others => 1);
   --% node.f_expr.p_aggregate_params

   C := (Value => 0, Succ|Pred => new Cell'(0, null, null));
   --% [x.p_aggregate_params for x in node.findall(lal.Aggregate)]
   C := (Value => 0, Succ => new Cell'(0, null, null), Pred => new Cell'(0, null, null));
   --% [x.p_aggregate_params for x in node.findall(lal.Aggregate)]
   C := (Value => 0, Succ|Pred => <>);
   --% node.f_expr.p_aggregate_params

   R := (A => 9);
   --% node.f_expr.p_aggregate_params
   R := (others => 9);
   --% node.f_expr.p_aggregate_params

   E := (A|B => 0);
   --% node.f_expr.p_aggregate_params
   E := (A => 0, B => 0);
   --% node.f_expr.p_aggregate_params
   E := (0, 0);
   --% node.f_expr.p_aggregate_params
end RecAgg;
