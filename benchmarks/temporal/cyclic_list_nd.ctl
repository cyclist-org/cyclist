fields: next;
precondition: List(x,x);
property: AG(List(x,x));
while x=x do
if * then
  x:=x.next
else
  y:=new();
  y.next:=x
fi
od