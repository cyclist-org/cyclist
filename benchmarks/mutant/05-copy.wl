fields: tl;
precondition: ls(c) ;
if c=nil then
  z := nil
else
  y := c; 
  z := new(); 
  z.tl := nil; 
  y := y.tl; 
  while y!=nil do
    d := new(); 
    d.tl := nil; 
    z.tl := d;
    z := d;
    y := y.tl
  od
fi

