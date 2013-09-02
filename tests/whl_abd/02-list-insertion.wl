fields: next;
precondition: ls(x) * z->z' ;
if x=nil then 
  stop 
fi ;
y := x.next;
while y!=nil do
  if * then 
    x.next := z;
    z.next := y;
    stop
  else
    x := y;
    y := y.next
  fi
od
