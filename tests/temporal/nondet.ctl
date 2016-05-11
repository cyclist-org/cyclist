fields: this;
precondition: true->TRUE * false->FALSE * x=TRUE * TRUE!=FALSE;
property: AF(AG(x=TRUE));
while * do 
  x:=true.this
od;
x:=false.this;
x:=true.this;
while x=x do
  skip
od
