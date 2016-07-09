fields: next;
precondition: x!=nil * ls(x,nil);
property: AG(x!=nil);
while x=x do
  if * then
    temp:=new();
    temp.next:=x;
    x:=temp
  else
    temp:=new();
    temp.next:=x;
    x:=temp
  fi
od;
while x=x do
  skip
od