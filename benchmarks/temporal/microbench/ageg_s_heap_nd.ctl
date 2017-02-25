fields: left,right;
precondition: x!=nil * bt(x);
property: AG(AF(x!=nil));
while x=x do
  if * then
    temp:=new();
    temp.left:=x;
    temp.right:=nil;
    x:=temp
  else
    x:=nil
  fi
od