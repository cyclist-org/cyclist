fields: next;
precondition: ls(y,x) * ls(x,nil) ;
property: AG(ls(y,x) * ls(x,nil));
while x!=nil do
  x:=x.next
od;
while x=x do
  skip
od