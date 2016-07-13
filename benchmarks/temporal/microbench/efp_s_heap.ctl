fields: next;
precondition: one!=two * x->one * one->two;
property: EF(btthree(x));
while x=x do
  if * then
    temp:=x;
    x:=new();
    x.next:=temp
  else
    temp:=x.next;
    free(x);
    x:=temp
  fi
od