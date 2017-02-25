fields: next;
precondition: ls(y,x) * ls(x,nil);
property: AG(EF(x=nil));
while x!=nil do
if * then
   temp:=x;
   x:=new();
   x.next:=temp
else
  if x!=nil then
    x:=x.next
  else
    skip
  fi
fi
od;
x:=nil;
while x=x do
  skip
od