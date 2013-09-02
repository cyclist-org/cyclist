fields: left, right;
precondition: bt(x) ;
while x!=nil do
  if * then
    x := x.left 
  else
    x := x.right
  fi 
od
