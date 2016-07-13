fields: next;
precondition: z->b * ls(x,nil);
property: AG(EF(ls(x,nil) * z->a));
while * do
  y:=new();
  y.next:=x;
  x:=y
od;
z.next:=a