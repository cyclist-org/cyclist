fields: next,prev;
precondition: flag1->a' * flag2->b' * flag3->c' * five->nil,four * four->five,three * three->four,two * two->three,one * one->two,nil * WItemsNum->three,one;
property: AG(EF(WItemsNum!=nil));
while flag1!=nil do
    while flag2!=nil do
	current:=WItemsNum.next;
        if current=four then
	    free(flag2);
	    flag2:=nil
	else
	    skip
	fi;
	if flag2!=nil then
	    if current=nil then
	        WItemsNum.next:=two;
		WItemsNum.prev:=nil
	    else
		nNum:=WItemsNum.next;
		nnNum:=nNum.next;
	        WItemsNum.next:=nnNum;
		WItemsNum.prev:=nNum
	    fi
	else
	    skip
	fi
    od;
    while flag3!=nil do
	current:=WItemsNum.next;
	if current=one then
	    free(flag3);
	    flag3:=nil
	else
	    pNum:=WItemsNum.prev;
	    ppNum:=pNum.prev;
            WItemsNum.next:=pNum;
	    WItemsNum.prev:=ppNum
	fi
    od;
    WItemsNum.next:=two;
    WItemsNum.prev:=nil
od;
while WItemsNum=WItemsNum do
    skip
od

