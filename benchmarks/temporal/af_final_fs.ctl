fields: next;
precondition: ls(y,x) * ls(x,nil) ;
property: AF(final);
while x!=nil do
  x:=x.next
od