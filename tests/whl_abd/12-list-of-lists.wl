fields: next, down;
precondition: lsls(x) ;
while x!=nil do
  y := x.down;
  while y!=nil do
    y := y.down
  od ;
  x := x.next
od
