fields: tl;
precondition: ls(c) ;
h:=c; 
c:=c.tl; 
p:=h;
while c!=h do
  o:=c; 
  c:=c.tl;
  if * then
    e:=o.tl; 
    p.tl:=e; 
    if * then 
      o.tl := e 
    fi
  else 
    p:=o
  fi
od

