fields: next;
precondition: ls(x,nil) ;
property: AG(ls(x,nil));
while x=x do
  while * do
    y:=new;
    y.next:=x;
    x:=y
  od
od