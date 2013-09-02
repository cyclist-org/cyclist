fields: next;
precondition: ls(x) ;
if x=nil then 
  stop 
fi ;
y := x.next;
if y=nil then
  if * then 
    free(x) 
  fi ; 
  stop
fi ;
z := y.next ;
while z!=nil do
  if * then
    x := y 
  else
    free(y) ;
    x.next := z 
  fi ;
  y := z ; 
  z := z.next
od
