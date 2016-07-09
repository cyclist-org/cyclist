fields: next;
precondition: ls(x,nil);
property: AG(EF(emp));
while x=x do
    if * then
        while x!=nil do
            temp:=x.next;
            free(x);
            x:=temp
        od
    else
        skip
    fi
od