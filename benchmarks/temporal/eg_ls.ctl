fields: next;
precondition: ls(x,nil) ;
property: AG(ls(x,nil));
while x=x do
  while x!=nil do
    skip
  od
od