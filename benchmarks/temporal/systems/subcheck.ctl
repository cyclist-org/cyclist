fields: next;
precondition: flag=true * a->zero * r->zero * ls(x,nil);
property:AG(flag=false);
while x=x do
a.next:=one;
while x!=nil do
      temp:=x;
      x:=x.next;
      free(temp)
od;
r.next:=one
od