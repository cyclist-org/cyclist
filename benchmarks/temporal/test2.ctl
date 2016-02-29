fields: next;
precondition: ls(x,nil);
property: AG ls(x,nil);
while x!=nil do
  x := x.next
od
