proc check(x)
	precondition: tree(x); 
	postcondition: tree(x);
{	
	if x=nil then
		return;
	else 
		(
		read(x);
		)
		||
		(
		read(x);
		)
	fi 
}
