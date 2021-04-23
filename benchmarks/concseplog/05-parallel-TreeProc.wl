proc proc(x)
	precondition: tree(x); 
	postcondition: tree(x);
{	
	if x=nil then
		return;
	else 
		(
		print(x->d);
		proc(x->l);
		proc(x->r)
		)
		||
		(
		print(x->d);
		proc(x->l);
		proc(x->r)
		)
	fi 
}
