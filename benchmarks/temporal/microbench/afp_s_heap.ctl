fields: next;
precondition: p->zero * ls(y,x) * ls(x,nil);
property: AF(p->one * ls(y,nil));
while x!=nil do
  x:=x.next
od;
p.next:=one;
while x=x do
  skip
od