fields: this;
precondition: x=y*z!=y*z->a';
property: AF(EG(x=y));
x:=z;
while * do
    x:=y
od;
x:=z;
x:=y;
while x=x do
      skip
od