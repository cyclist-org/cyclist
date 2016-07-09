fields: next;
precondition: ls(x,nil);
property: AG(EF(x=nil));
while x!=nil do
if * then
   temp:=x;
   x:=new();
   x.next:=temp
else
  x:=x.next
fi
od;
x:=nil;
while x=x do
  skip
od