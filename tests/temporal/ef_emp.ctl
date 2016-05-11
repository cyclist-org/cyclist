fields: next;
precondition: x->nil;
property: EF(emp);
temp:=new();
temp:=x;
while x!=nil do
      free(x);
      x:=temp
od;
free(temp)