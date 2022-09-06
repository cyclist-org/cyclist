fields: next, right, left;


proc print(x) 
	precondition: tree(x); 
	postcondition:  tree(x); 
{	
	return
}


proc pro(x)
	precondition: tree(x); 
	postcondition: tree(x);
{	
	if x=nil then
		return
	else 
		( 
			d := x.next;
			print(d);
			l := x.left;
			pro(l);
			r := x.right;
			pro(r) 
		|| 
			d := x.next;
			print(d);
			l := x.left;
			pro(l);
			r := x.right;
			pro(r) 
		)
	fi 
}


proc main(x) 
	precondition: tree(x); 
	postcondition: tree(x);
{	
	proc(x)
}
