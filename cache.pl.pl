convertBinToDec(String, Decimal) :-
    string_chars(String,Chars),
    convertBinToDec_(Chars,0,Decimal).

convertBinToDec_(['1'|Cs],N,Decimal) :-
    M is 2 * N + 1,
    convertBinToDec_(Cs,M,Decimal).    

convertBinToDec_(['0'|Cs],N,Decimal) :-
    M is 2 * N,
    convertBinToDec_(Cs,M,Decimal).    

convertBinToDec_([],M,M).


replaceIthItem(Item,List,I,Result):-
    replaceIthItem_(Item,List,I,Result,0).

replaceIthItem_(_,[],_,[],_).

replaceIthItem_(Item,[_|T],Pos,[Item|T1],Pos):-
    Pos1 is Pos + 1,
    replaceIthItem_(Item,T,Pos,T1,Pos1).

replaceIthItem_(Item,[H|T],I,[H|T1],Pos):-
    I \== Pos,
    Pos1 is Pos + 1,
    replaceIthItem_(Item,T,I,T1,Pos1).
    

splitEvery(N,List,R):- 
    splitEvery_(N,List,R,0).

splitEvery_(_,[],[],_).
splitEvery_(N,List,[R1|T],Pos):-
    splitN(N,List,R1,Pos,Rest),
    splitEvery_(N,Rest,T,0).
    

splitN(N,T,[],N,T).
splitN(_,[],[],_,[]).
splitN(N,[H|T],[H|T1],Pos,A):-
    Pos < N,
    Pos1 is Pos + 1,
    splitN(N,T,T1,Pos1,A).
    
logBase2(Num,Res):- 
    Res is integer(round(log10(Num)/log10(2))).

    
             
	
fillZeros(String,N,R):-
    zeroString(N,"",L),
    string_concat(L,String,R).
    
zeroString(0,R,R).       
zeroString(N,L,R1):-
    N > 0,
    N1 is N - 1,
    string_concat(L,"0",R),
    zeroString(N1,R,R1). 

getNumBits(_,fullyAssoc,_,0).

getNumBits(0, setAssoc, _, 1).
getNumBits(N,setAssoc,_,BitsNum):-
    logBase2(N,BitsNum).

getNumBits(_,directMap,Cache,BitsNum):-
    getNumBits_(_,directMap,Cache,N,1),
    logBase2(N,BitsNum).


getNumBits_(_,directMap,[],Pos,Pos).
getNumBits_(_,directMap,[_|T],BitsNum,Pos):-
    Pos1 is Pos + 1,
    getNumBits_(_,directMap,T,BitsNum,Pos1).


 getDataFromCache(StringAddress, Cache, Data, Decimal, setAssoc, SetsNum):-
    logBase2(SetsNum, Numindexbits),
    number_string(N, StringAddress),
    S is N mod (10 ** Numindexbits),
    convertBinToDec(S, Decimal),
    sub_string(StringAddress, _, _, Numindexbits, Tagstring),
    length(Cache, Cachesize),
    ceiling(Cachesize/SetsNum, M),
    splitEvery(M, Cache, Cachesplit),
    getset(Cachesplit, Decimal, Set),
    !,
    getdata(Set, Tagstring, Data).

getset(Cache, Index, Set):-
               getset_(Cache,Index, Set,0).
               
getset_([_ | T], Index, Set, Pos):-
              	Index \== Pos,
               Pos1 is Pos + 1,
               	getset_(T, Index, Set, Pos1).
               
getset_([H | _], Index, H, Index).
           

getdata([item(tag(H), data(_), _, _) | T], Tag, Data):-

    H \== Tag,
    getdata(T, Tag, Data).
    
    

getdata([item(tag(Tag), data(Data), 1, _) | _], Tag, Data).


convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
    logBase2(SetsNum, Numindexbits),
     Tag is Bin // (10 ** Numindexbits),
   	Idx is Bin mod (10 ** Numindexbits).
   


getDataFromCache(StringAddress,Cache,Data,HopsNum,fullyAssoc,_):-
    getDataFromCache_(StringAddress,Cache,Data,HopsNum,fullyAssoc,_,0).

getDataFromCache_(StringAddress,[item(tag(H),_,_,_)|T],Data,HopsNum,fullyAssoc,_,Pos):-
    H \== StringAddress,              
    Pos1 is Pos + 1,
    getDataFromCache_(StringAddress,T,Data,HopsNum,fullyAssoc,_,Pos1).

getDataFromCache_(StringAddress,[item(tag(StringAddress),data(Data),1,_)|_],Data,Pos,fullyAssoc,_,Pos).


convertAddress(String,_,String,_,fullyAssoc).
    


%ReplaceInCache For Direct Mapping

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,_) :-
    string_concat(Tag,Idx,Tot),
    convertBinToDec(Idx,Drep),
    convertBinToDec(Tot, Didx),
    nth0(Didx, Mem, ItemData),
    getNumBits(_, directMap,_,BitsNum),
    Needed is 3 - BitsNum,
    fillZeros(Tag,Needed,Tag4),
    replaceIthItem((item(tag(Tag4),data(ItemData),1,0)), OldCache,Drep, NewCache).


%ReplaceInCache_For_FullyAssociative
%replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum)

maxOrder([],Pos,_,_,Pos).
maxOrder(Cache,Pos3):-
    maxOrder(Cache , Pos , 0, -1,Pos3).
    
maxOrder([item(_,_,_,O1)|T],Pos, Pos1, MaxOrd,Pos3):-
    Pos2 is Pos1 + 1,
    O1 > MaxOrd,
    maxOrder(T , Pos1 , Pos2, O1,Pos3). 
    
maxOrder([item(_,_,_,O1)|T],Pos, Pos1, MaxOrd,Pos3):-
    Pos2 is Pos1 + 1,
    O1 < MaxOrd,
    maxOrder(T , Pos, Pos2, MaxOrd,Pos3).   


indexGet([],_,-1).
indexGet([item(_,_,0,_)|_],I,I).
indexGet([item(_,_,V1,_)|T],I,R) :-
    V1 \==0,
    I1 is I + 1,
    indexGet(T,I1,R).

orderAdder([],_).
orderAdder([item(Ta,D,V,O)|T],R):-
        V == 0,
        append(R,[item(Ta,D,V,O)],Lis),
        orderAdder(T,Lis).
orderAdder([item(Ta,D,V,O)|T],R):-
        V == 1,
        O1 is O + 1,
        append(R,[item(Ta,D,V,O1)],Lis),
        orderAdder(T,Lis).



replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :- 
 
    indexGet(OldCache,0,ValidityPos),
    ValidityPos == -1,
    maxOrder(OldCache,PosOrd),
    orderAdder(OldCache,Res),
    convertBinToDec(Tag,TD),
    nth0(TD, Mem, ItemData),
    fillZeros(Tag,4,Tag4),
    replaceIthItem((item(tag(Tag4),data(ItemData),1,0)), OldCache,PosOrd, NewCache).

replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :- 
    indexGet(OldCache,0,ValidityPos),
    ValidityPos \== -1,
    orderAdder(OldCache,Res),
    convertBinToDec(Tag,TD),
    nth0(TD, Mem, ItemData),
    fillZeros(Tag,4,Tag4),
    replaceIthItem((item(tag(Tag4),data(ItemData),1,0)), OldCache,ValidityPos, NewCache).


%ReplaceInCache_For_FullyAssociative
%replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum)

maxOrder([],Pos,_,_,Pos).
maxOrder(Cache,Pos3):-
    maxOrder(Cache , Pos , 0, -1,Pos3).
    
maxOrder([item(_,_,_,O1)|T],Pos, Pos1, MaxOrd,Pos3):-
    Pos2 is Pos1 + 1,
    O1 > MaxOrd,
    maxOrder(T , Pos1 , Pos2, O1,Pos3). 
    
maxOrder([item(_,_,_,O1)|T],Pos, Pos1, MaxOrd,Pos3):-
    Pos2 is Pos1 + 1,
    O1 < MaxOrd,
    maxOrder(T , Pos, Pos2, MaxOrd,Pos3).   


indexGet([],_,-1).
indexGet([item(_,_,0,_)|_],I,I).
indexGet([item(_,_,V1,_)|T],I,R) :-
    V1 \==0,
    I1 is I + 1,
    indexGet(T,I1,R).

orderAdder([],_).
orderAdder([item(Ta,D,V,O)|T],R):-
        V == 0,
        append(R,[item(Ta,D,V,O)],Lis),
        orderAdder(T,Lis).
orderAdder([item(Ta,D,V,O)|T],R):-
        V == 1,
        O1 is O + 1,
        append(R,[item(Ta,D,V,O1)],Lis),
        orderAdder(T,Lis).



replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :- 
 
    indexGet(OldCache,0,ValidityPos),
    ValidityPos == -1,
    maxOrder(OldCache,PosOrd),
    orderAdder(OldCache,Res),
    convertBinToDec(Tag,TD),
    nth0(TD, Mem, ItemData),
    fillZeros(Tag,4,Tag4),
    replaceIthItem(item(tag(Tag4),data(ItemData),1,0), Res,PosOrd, NewCache).

replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :- 
    indexGet(OldCache,0,ValidityPos),
    ValidityPos \== -1,
    orderAdder(OldCache,Res),
    convertBinToDec(Tag,TD),
    nth0(TD, Mem, ItemData),
    fillZeros(Tag,4,Tag4),
    replaceIthItem((item(tag(Tag4),data(ItemData),1,0)), Res,ValidityPos, NewCache).



%ReplaceInCache_For_SetAssociative
%splitEvery_for_it??

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum) :-
    stringconcat(Tag,Idx,Tot),
    convertBinToDec(Tot, Didx),
    nth0(Didx, Mem, ItemData),
    convertBinToDec(Idx, SetIdx), 
    length(OldCache, Cachesize),
    ceiling(Cachesize/SetsNum,M),
    splitEvery(M, OldCache, Cachesplit),
    getset(Cachesplit, SetIdx, Set),
    replaceInCache(Tot,,Mem,Set,SCache,ItemData,fullyAssoc,_),
    replaceIthItem(SCache, Cachesplit, SetIdx,NewCache).


getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
Type,NumOfSets).

