fields: tl;
precondition: ls(c);
h:=c; 
c:=c.tl;
while c!=h do
  t:=c;
  c:=c.tl;
  free(t)
od ;
h.tl := h
