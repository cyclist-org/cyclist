fields: next;
precondition: a->zero * r->zero * ls(x,nil);
property:AF(a->one * r->one * emp);
while * do
  if * do 
    a.next:=one
  else
    skip
  fi
od
while x!=nil do
      temp:=x;
      x:=x.next;
      free(temp)
od;
r.next:=one