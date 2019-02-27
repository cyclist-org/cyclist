fields: left,right;
precondition: x!=nil * bt(x);
property: AG(EG(x!=nil));
while x=x do
    temp:=new();
    temp.left:=x;
    temp.right:=nil;
    x:=temp
od