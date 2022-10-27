fields: next;
  
{ [a][pi](tree x)}
proc main(x)
	precondition: [a][pi] tree(x); 
	postcondition: [a][pi] tree(x);
{	 
	if x = nil then
		{ x = nil }  /\  {[a][pi] tree(x)}
		return
	   {[a][pi] tree(x)}
	else 
		{ x != nil }  /\  {[a][pi] tree(x)}
		{ x != nil }  /\  {[a][0.5*pi] tree(x) (*) [a][0.5*pi] tree(x)}
		{ x != nil }  /\  {a = b (*) b : [b][0.5*pi] tree(x) (*) [b][0.5*pi] tree(x)}
		(
		{ x != nil }  /\  {a = b (*) b : [b][0.5*pi] tree(x)}
		read(x)
		{ x != nil }  /\  {a = b (*) b : [b][0.5*pi] tree(x)}
		|| 
		{ x != nil }  /\  {a = b (*) b : [b][0.5*pi] tree(x)}
		read(x)
		{ x != nil }  /\  {a = b (*) b : [b][0.5*pi] tree(x) 
		)
		{ x != nil }  /\  {a = b (*) b : [b][0.5*pi] tree(x) (*) [b][0.5*pi] tree(x)}
		{ x != nil }  /\  {[a][0.5*pi] tree(x) (*) [a][0.5*pi] tree(x)}
		{ x != nil }  /\  {[a][pi] tree(x)}
	fi 
}
