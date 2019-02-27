fields: next;
precondition: flag=false * a->zero * r->zero * ls(x,nil);
property:AG(AF(a->one * r->one * emp));
while x=x do
flag:=true;
a.next:=one;
while x!=nil do
      temp:=x;
      x:=x.next;
      free(temp)
od;
r.next:=one
od