fields: tl;
precondition: ls(c) ;
y := c; 
flag := c; 
while flag!=nil do
  if * then 
    flag := nil 
  fi;
  y := y.tl;
  if y=nil then 
    flag := nil 
    fi
od ;
a:=y;
if y!=nil then 
  y:=y.tl 
fi ;
flag:=y; 
while flag!=nil do
  t := y; 
  y:=y.tl;
  free(t);
  if * then 
    flag:=nil 
  fi ;
  if y=nil then 
    flag:=nil 
  fi
od ;
if a!=nil then 
  a.tl:=y 
fi
