fields: next;
precondition: ls(x,nil) ;
property: AF(final);
while x!=nil do
  temp:=x;
  x:=x.next;
  free(temp)
od