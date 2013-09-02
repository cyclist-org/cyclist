fields: left, right, next;
precondition: tl(x) ;
while * do
  if x = nil then 
    stop 
  fi;
  if * then
    x := x.next;
    while x!=nil do
      x := x.next
    od ;
    stop
  fi;
  if * then
    x := x.left 
  else
    x := x.right
  fi 
od
