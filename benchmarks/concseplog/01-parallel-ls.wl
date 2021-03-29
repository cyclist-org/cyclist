fields: next;

proc main(x, y)
	precondition: ls(y,x) * ls[a](x,nil); 
	postcondition: ls(y,nil);
{
	z := x;
	assert (ls(y,z) * ls[a](z,nil));
	(while z!=nil do
		z := z.next;
		assert([b'] < [a] : ls(y, z) * ls[b'](z,nil))
	od)
	||
	(while z!=nil do
		z := z.next;
		assert([b'] < [a] : ls(y, x) * ls[b'](z,nil))
	od)
}
