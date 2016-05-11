fields: tl;
precondition: cls(c) ;
h:=c; 
c:=c.tl;
while c!=h do
  c:=c.tl
od

