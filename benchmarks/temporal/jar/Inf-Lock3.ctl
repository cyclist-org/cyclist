fields: next;
precondition: flag=false * a->zero * r->zero * ls(x,nil);
property:AG(\/(flag=false,AF(a->one * r->one * emp)));
while x=x do
  if * then
    flag:=true;
    a.next:=one;
    while x!=nil do
      temp:=x;
      x:=x.next;
      free(temp)
    od;
    r.next:=one
  else
    flag:=false
  fi
od