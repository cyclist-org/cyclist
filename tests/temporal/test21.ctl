fields: next;
precondition: y=nil * ls(x,nil);
property: EF y->a;
while x!=nil do
  x := x.next
od;
y:=b
