fields: left, right, next;
precondition: lsls(x) ;
while x!=nil do
  y := x.left;
  while y!=nil do
    if * then
      y := y.left
    else 
      y := y.right
    fi
  od ;
  x := x.next
od
