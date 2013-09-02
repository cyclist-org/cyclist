fields: next;
precondition: ls(x) * ls(y) ;
if x=nil then 
  x := y ; 
  stop 
fi ;
z := x.next;
while z!=nil do
  x := z;
  z := x.next
od ;
x.next := y
