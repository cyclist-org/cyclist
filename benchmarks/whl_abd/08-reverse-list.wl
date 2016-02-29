fields: next;
precondition: ls(x) ;
y := nil;
while x!=nil do
  z := x;
  x := x.next;
  z.next := y;
  y := z
od
