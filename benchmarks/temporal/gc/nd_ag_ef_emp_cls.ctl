fields: next;
precondition: cls(x);
property: AG(EF(emp));
while x=x do
    if * then
        while x!=nil do
            temp:=x.next;
            free(x);
            x:=temp;
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