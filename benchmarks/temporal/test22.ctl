fields: next;
precondition: ls(x,nil);
property: AF y->a;
while x!=nil do
  x := x.next
od;
y:=new();
skip
