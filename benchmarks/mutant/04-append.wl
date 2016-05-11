fields: tl;
precondition: ls(x) * ls(y) ;
if x=nil then
  x := y
else
  t := x;
  n := t.tl;
  while n != nil do
    t := n;
    n := t.tl
  od ;
  t.tl := y
fi
