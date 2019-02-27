fields: left,right;
precondition: x!=nil * bt(x);
property: AG(AF(bt(x)));
while x=x do
    temp:=new();
    temp.left:=x;
    temp.right:=nil;
    x:=temp
od