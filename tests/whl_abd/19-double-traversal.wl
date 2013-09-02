fields: next;
precondition: ls(x) ;
y:=x;
while y!=nil do 
  y := y.next 
od ; 
while x!=nil do 
  x := x.next 
od

