fields: next;
precondition: ls(x,nil);
property: AG(AF(emp));
while x=x do
  if * then
     while x!=nil do
       temp:=x;
       x:=x.next;
       free(temp);
       temp:=nil
     od
  else
    while * do
       y:=new();
       y.next:=x;
       x:=y
     od	
  fi
od