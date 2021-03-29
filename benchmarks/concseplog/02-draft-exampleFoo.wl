proc foo(x, y)
	precondition: ls(x,y); 
	postcondition: ls(x,y);
{
	if x=y then
		return;
	else 
		foo([x],y);
	fi	
}
