fields: next;
precondition: temp=x * a=nil * r=nil * flag->f' * x->x' * three->two * two->one * one->nil;
property: AG(\/(a=nil,EF(r!=nil)));
while flag!=nil do
    if * then
	free(flag);
        flag:=nil
    else
	skip
    fi;
    if flag!=nil then
	x:=new();
	x.next:=one;
        a:=new();
	free(a);
	a:=nil;
	while x!=nil do
	    temp:=x;
	    x:=x.next;
	    free(temp);
	    temp:=nil
	od;
	r:=new();
	free(r);
	r:=nil
    else
	skip
    fi
od;
while flag=flag do
    skip
od
