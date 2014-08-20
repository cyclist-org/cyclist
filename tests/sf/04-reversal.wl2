fields: next;
precondition: ls(x,nil) * ls(y,nil);
postcondition: ls(x,nil) * ls(y,nil);
while x!=nil do
  z := x;
  x := x.next;
  z.next := y;
  y := z
od
