fields: next;
precondition: ls(x) ;
x := x.next;
while x!=nil do
  x := x.next;
  x := x.next
od