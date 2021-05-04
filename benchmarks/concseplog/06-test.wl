fields: next;

proc main(x, y)
	precondition: ls(y,x) * ls[a](x,nil); 
	postcondition: ls(y,nil);
{
	z := x;
	assert (ls(y,z) * ls[a](z,nil));
	( 
	z := x;
	|| 
	z := x;
	)
}
