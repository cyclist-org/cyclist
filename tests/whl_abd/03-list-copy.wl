fields: next;
precondition: ls(x) ;
if x=nil then 
  stop 
fi ;
y := new();
x := x.next;
while x!=nil do 
  z := new();
  y.next := z;
  y := z;
  x := x.next
od ;
y.next := nil
