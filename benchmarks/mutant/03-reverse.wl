fields: tl;
precondition: ls(c) ;
p := nil;
while c!=nil do
  n := c.tl;
  c.tl := p;
  p := c;
  c := n
od