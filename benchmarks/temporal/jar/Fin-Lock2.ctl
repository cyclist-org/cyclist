fields: next;
precondition: a->zero * r->zero * ls(x,nil);
property:AG(AF(a->one * r->one * emp));
a.next:=one;
while x!=nil do
      temp:=x;
      x:=x.next;
      free(temp)
od;
r.next:=one