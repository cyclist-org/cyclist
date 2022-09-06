 

{ [a][1.0](z |-> nil) /\ [b][p1](ls x nil) }
proc copy(x, z) {
	l := [z];
	{ x = nil }
		\/
	{b = b1*b2, b1#b2 : x != nil : [b1][p1](x |-> x') * [b2][p1](ls x' nil) }
	while (x != nil) {
		tmp := l;
		l := new();
		[l] := tmp;
		x := [x];
	}
	// Folding up of b1 * b2 into b again
	[z] := l;
	return;
}
{ exists v . [a][1.0](z |-> v) * [b][p1](ls x nil) * [c][1.0](ls v nil) }
	// implies
{ a#b, a#c, b#c : exists v . [a][p1](ls x nil) * [b][1.0](z |-> v) * [c][1.0](ls v nil) }

{ [a][1.0](ls x nil) }
z1 := new()
[z1] := nil
z2 := new()
[z2] := nil
{ [a][1.0](ls x nil) * [b][1.0](z1 |-> v1) * [d][1.0](z2 |-> v2) }
// Here we have split [a][1.0](ls x nil) into [a][0.5](ls x nil) * [a][0.5](ls x nil)
(copy(x, z1) || copy (x, z2))
...
{ [a][1.0](ls x nil) * [b][1.0](z1 |-> v1) * [c][1.0](ls v1 nil) * [d][1.0](z2 |-> v2) * [e][1.0](ls v2 nil) }