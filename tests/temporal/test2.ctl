fields: next;
precondition: ls(x,nil);
property: <>x=nil;
while x!=nil do
  x := x.next
od
