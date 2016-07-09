fields: next;
precondition: ls(x,nil);
property: AF(emp);
while x!=nil do
      temp:=x.next;
      free(x);
      x:=temp
od