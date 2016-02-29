fields: left, right;
precondition: bt(x) ;
while x!=nil do
  if * then
    y := x.left ;
    if y=nil then 
      y := new();
      x.left := y;
      stop
    fi
  else
    y := x.right ;
    if y=nil then 
      y := new();
      x.left := y;
      stop
    fi
  fi ;
  x := y
od
