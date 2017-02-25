fields: next,prev;
precondition: WItemsNum!= nil *WItemsNum->nil,nil;
property: EF(AG(WItemsNum!= nil *WItemsNum->nil,nil));
current:=WItemsNum.next;
while current=nil do
    skip
od;
while flag1!=nil do
    while flag2!=nil do
	current:=WItemsNum.next;
        if current=nil then
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
	        WItemsNum.next:=nnum;
		WItemsNum.prev:=nNum
	    fi
	else
	    skip
	fi
    od;
    while flag3!=nil do
	current:=WItemsNum.next;
	if current=nil then
	    free(flag3);
	    flag3:=nil
	else
	    pNum:=WItemsNum.prev;
	    ppNum:=pNum.prev;
            WItemsNum.next:=pNum;
	    WItemsNum.prev:=ppNum
	fi
    od
od;
while WItemsNum=WItemsNum do
    skip
od
