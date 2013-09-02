fields: next;
precondition: ls(x) ; 
while x!=nil do
  y := x;
  x := x.next;
  free(y)
od
