fields: next;
precondition: a=nil * r=nil * flag->f' * x->x' * three->two * two->one * one->nil;
property: /\(EF(a!=nil),EF(AG(r=nil)));
while flag!=nil do
    if * then
	free(flag);
        flag:=nil
    else
	skip
    fi;
    if flag!=nil then
	x:=new();
	x.next:=three;
        a:=new();
	free(a);
	a:=nil;
	while x=x do
	    skip
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
