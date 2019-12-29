function Random_Nat return Natural;

function Incr (X : Natural) return Natural
   with Post => Incr'Result = X + 1;

function Greater_1 (X : Natural) return Natural
   with Post => Greater_1'Result > X;

function Greater_2 (X : Natural) return Natural
   with Post => Greater_2'Result > X + 1;

function Greater_3 (X : Natural) return Natural
   with Post => Greater_3'Result > X and Greater_3'Result > X + 1;

function Greater_4 (X : Natural) return Natural
   with Post => Greater_4'Result > X + 1 and Greater_4'Result > X;

function Greater_5 (X : Natural; Y : Natural) return Natural
   with Post => Greater_5'Result > X and Greater_5'Result > Y;
