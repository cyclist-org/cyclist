fields: next;
precondition: ls(y,x) * ls(x,nil);
property: AF(EG(x->y));
while x!=nil do
  temp:=x.next;
  free(x);
  x:=temp
od;
x:=new();
x.next:=y;
x:=y;
while TRUE=TRUE do
  if * then
   x:=x.next
  else
    skip
  fi
od
