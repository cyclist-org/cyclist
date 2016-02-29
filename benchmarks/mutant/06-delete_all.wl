fields: tl;
precondition: ls(c) ;
while c!=nil do
  t:=c;
  c:=c.tl;
  free(t)
od

