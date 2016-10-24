fields: next;
precondition: y->a * a->nil * a'->b' * ls(x,nil);
property: AF (y->nil);
while x!=nil do
  x := x.next
od;
y:=y.next
