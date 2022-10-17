fields: next;

proc main(x, y)
	precondition: ls(y,x) * ls(x,nil)[a][1/2]; 
	postcondition: ls(y,nil);
{
	 
	z := x;
	(
	assert (ls(y,z) * ls[a](z,nil)) 
	||
	assert (ls(y,z) * ls[a](z,nil)) 
	)
}
