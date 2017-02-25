fields: next;
precondition:ls(x,nil);
property: AF(AG(emp));
while x=x do
    if * then
        while x!=nil do
            temp:=x.next;
            free(x);
            x:=temp
        od
    else
        while * do
	    y:=new();
	    y.next:=x;
	    x:=y
	od
    fi
od