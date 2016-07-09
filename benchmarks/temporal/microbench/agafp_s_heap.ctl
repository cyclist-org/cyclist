fields: next;
precondition: one!=zero * ls(x,one) * one->zero;
property: AG(AF(x!=zero));
if * then
   temp:=x;
   x:=new();
   x.next:=temp
else
  while x!=one do
    temp:=x.next;
    free(x);
    x:=temp
  od;
  x:=one
fi;
while x=x do
  skip
od
