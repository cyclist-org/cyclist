fields: tl;
precondition: ls(c) ;
h := c;
x := new();
if c!=nil then 
  p := c;
  c := c.tl;
  while c!=nil do
    if * then
      x.tl := c;
      p.tl := x;
      c := nil 
    else
      p := p.tl;
      c := c.tl
    fi
  od ;
  x.tl := nil;
  p.tl := x
else
  x.tl := nil;
  c := x
fi
