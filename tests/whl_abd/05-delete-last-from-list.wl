fields: next;
precondition: ls(x) ;
if x=nil then 
  stop 
fi ;
y := x.next;
if y=nil then 
  free(x) ; 
  stop 
fi ;
z := y.next;
while z!=nil do
  x := y;
  y := z;
  z := z.next
od ;
free(y);
x.next := nil
