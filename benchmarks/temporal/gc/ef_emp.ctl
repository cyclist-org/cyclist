fields: next;
precondition: ls(x,nil);
property: EF(emp);
while x!=nil do
      temp:=x.next;
      free(x);
      x:=temp
od