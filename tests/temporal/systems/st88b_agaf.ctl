fields: next;
precondition: flag1->a' * flag2->b' * flag3->c' * WItemsNum->five * five->four * four->three * three->two * two->one * one->nil;
property: AG AF WItemsNum!=nil;
while flag1!=nil do
    while flag2!=nil do
	current:=WItemsNum.next;
        if current=four then
	    free(flag2);
	    flag2:=nil
	else
	    skip
	fi;
	if current=four then
	    skip;
	    WItemsNum:=WItemsNum.next
	else
	    WItemsNum:=WItemsNum.next
	fi
    od;
    while flag3!=nil do
	current:=WItemsNum.next;
	if current=one then
	    free(flag3);
	    flag3:=nil
	else
	    WItemsNum:=WItemsNum.next
	fi
    od;
    WItemsNum:=two.next
od;
while WItemsNum=WItemsNum do
    skip
od

