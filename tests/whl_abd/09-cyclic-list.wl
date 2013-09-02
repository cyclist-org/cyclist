fields: next;
precondition: cls(x) ;
y := x;
x := x.next;
while x!=y do 
  x := x.next 
od
