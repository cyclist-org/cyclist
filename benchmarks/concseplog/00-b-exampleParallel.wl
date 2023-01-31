fields: next;

proc main(x, y)
	precondition: ls(y,x) * ls[a][1/2](x,nil); 
	postcondition: ls(y,nil);
{
	 
	z := x;
	(
	assert (ls(y,z) * ls[a](z,nil)) 
	||
	assert (ls(y,z) * ls[a](z,nil)) 
	)
}
