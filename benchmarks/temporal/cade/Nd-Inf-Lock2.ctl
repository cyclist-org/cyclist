fields: next;
precondition: one!=zero * a->zero * r->zero;
property:AG(AF(a->one * r->one));
while x=x do
  if * then
    a.next:=one;
    while * do
      skip
    od;
    r.next:=one
  else
    skip
  fi
od