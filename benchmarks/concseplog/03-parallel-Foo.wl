fields: next;

proc main(x, y)
	precondition: ls(x,y); 
	postcondition: ls(x,y); 
{
	(
		foo(x,y)
	||
		foo(x,y)
	)
}

proc foo(x, y)
	precondition: ls(x,y); 
	postcondition: ls(x,y);
{
	if x=y then
		return
	else 
		z := x.next;
		foo(z,y)
	fi 
}
