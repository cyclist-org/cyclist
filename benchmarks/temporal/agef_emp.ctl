fields: next;
precondition: ls(x,nil) ;
property: AG(EF(emp));
while x=x do
while x!=nil do
tmp:=x.next;
free(x);
x:=tmp
od
od