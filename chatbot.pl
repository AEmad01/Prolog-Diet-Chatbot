:- dynamic value_HL/1.


:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- style_check(-discontiguous).
:- ['read_sentence.pl'].
:- ['info_food.pl'].





response([quit]):-
    halt.



response([how,many,calories,does,X,contain],_,_,Y):-
foodCal(X,Y1),
not(Y1 = 0),
Y =[Y1,"Calories"].

response([how,many,calories,does,X,contain],_,_,Y):-
foodCal(X,Y1),
(Y1 = 0),
Y =["I", do, not, know].


 response([what,does,X,contain],PQ,PR1,R):-
 filterprop(contain,X,CR),
 writeln(CR),
 flatten(PR1,PR),
 getDiffAnswer([what,does,X,contain],PQ,PR,CR,R).


getcal([]).

getcal(T):-
getcal(T,CAL).

getcal([H|T],CAL) :- prop(H,_,CAL,_),
getcal(T,CAL1).



nth_pos(L, N, R):-
    nth_pos(L, 1, N, [], R).

nth_pos([], I, N, Acc, Acc).

nth_pos([H|T], I, N, Acc, R):-
    I =:= N,
    append(Acc, [H], Acc2),
    I2 is 1,   
    nth_pos(T, I2, N, Acc2, R).

nth_pos([H|T], I, N, Acc, R):-
    I < N,
    I2 is I + 1,   
    nth_pos(T, I2, N, Acc, R).

merge_list([],L,L ).
merge_list([H|T],L,[H|M]):-
    merge_list(T,L,M).

filterprop(X,CP):-
         findall(Y,prop(CP,X,Y),R),
         writeln(R).
filterprop(X,CP,R):-
         findall(Y,prop(CP,X,Y),R).


                 
                 
remove_last([], []) :- !, fail.

remove_last([_], []) :- !.

remove_last([X | T], [X | T2]) :-
    remove_last(T, T2).





matchFirst(_,[],RES).
matchFirst(T1,LF,LM):-
filterprop(contain,T1,R),
LF=[H|T],
H=(O,X),
member(X,R),
LM=[X-1|T2],
matchFirst(T1,T,T2).



matchFirst(T1,LF,LM):-
filterprop(contain,T1,R),
writeln(R),
LF=[H2|T],
H2=(O,X),
maplist(dif(X), R),
LM=[X-0|T2],
matchFirst(T1,T,T2).



matchSecond(_,[],RES).
matchSecond(T1,LF,LM):-
filterprop(contain,T1,R),
LF=[H|T],
H=(O,X),
member(O,R),
LM=[O-1|T2],
matchSecond(T1,T,T2).



matchSecond(T1,LF,LM):-
filterprop(contain,T1,R),
LF=[H2|T],
H2=(O,X),
maplist(dif(O), R),
LM=[O-0|T2],
matchSecond(T1,T,T2).

append1([],L,L).
append1([H|T],L,[H|T1]):- append1(T,L,T1).


test(U,T):-
mergefix(U,T).
mergefix([X-0,X-0],X-0).
mergefix([X-1,X-0],X-1).
mergefix([X-0,X-1],X-1).
mergefix([X-1,X-1],X-2).
mergefix(X,X).




mergeMatchLists(ML1,ML2,R):-
append(ML1,ML2,ML),
mergeMatchList1(ML,R).

mergeMatchList2(L-X,[L-Z|T],R):-
mergeMatchList2(L-X,T,R2),
R is Z+R2.

mergeMatchList2(L-X,[Q-Z|T],R):-
\+L=Q,
mergeMatchList2(L-X,T,R).
mergeMatchList2(L-X,[],X).

mergeMatchList1([],[]).
mergeMatchList1([L-X|T],[L-Z|T2]):-

mergeMatchList2(L-X,T,Z),

delete(T,L-_,Q),

mergeMatchList1(Q,T2).




remove_char(S,C,X) :- atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X).

        


bestMatchesMin([],_,[]).
bestMatchesMin([H-Z|T],Z,[H|T2]):-
bestMatchesMin(T,Z,T2).
bestMatchesMin([H-Z|T],K,T2):-
\+H=K,
bestMatchesMin(T,K,T2).




reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).



        

foodCal(X,Y):-
prop(X,contain,Y,cal).

foodCal(X,Y):-
filterprop(contain,X,R),
helper(R,Y).

foodCalL([],0).
foodCalL([H|T],F):-
foodCal(H,F1),
foodCalL(T,F2),
F is F1+F2.



helper([],0).
helper([H|T],C):-
prop(H,contain,X,cal),
helper(T,C1),
C is C1+X.


calcCalories(F,PQ,PR,C):-
foodCal(F,CAL),
C is 1800-F.

num_name(Name-Num, Num-Name).
name_num(Num-Name, Name-Num).

sort_names_by_num(UnsortedNameNum, SortedNameNum) :-
    maplist(num_name, UnsortedNameNum, UnsortedNumName),
    msort(UnsortedNumName, SortedNumName),
    maplist(name_num, SortedNumName, SortedNameNum).

listOrderDesc(L,X):-
sort_names_by_num(L,L1),
reverse(L1,X).

getDiffAnswer(_,_,PR1,CR1,R) :-
flatten(PR1,PR),
append(PR,[R|_],CR1),
delete(CR1,PR,CR).

getDiffAnswer(_,_,PR1,CR,R) :-
flatten(PR1,PR),
\+append(PR,[R|_],CR),
writeln(PR),
append(PR,[_|R],CR).

find(X,Y, [X,Y|_]).
find(X,Y, [X0,Y0|Xs]) :-
   dif(X+X0,Y+Y0),
   find(X,Y, [Y0|Xs]).

input_food([_Somebody, ate, Food| _Rest], Food).
input_food(Input, Food) :-
    append(_Something, [you, can, have, Food | _Rest], Input).
foodFromHistory([], []).
foodFromHistory([I|Is], [Food|Fs]) :-
    input_food(I, Food),
    foodFromHistory(Is, Fs).
foodFromHistory([I|Is], Fs) :-
    \+ input_food(I, _Food),
    foodFromHistory(Is, Fs).

hate_food([_Somebody, do, not, eat, Food| _Rest], Food).
getUnlikedIngredients([], []).
getUnlikedIngredients([I|Is], [Food|Fs]):-
    hate_food(I, Food),
    getUnlikedIngredients(Is, Fs).
getUnlikedIngredients([I|Is], Fs):-
    \+ hate_food(I, _Food),
    getUnlikedIngredients(Is, Fs).
                 
isValid(X):-
response(X).



% Put everything in a Read-eval-print loop
readInputTillQuit :-
        writeln('Welcome to your personal assistant'),     
        loop.

loop :-
        res(I), 
                delete(I,'.',X),
                delete(X,'[',Y),
                delete(Y,']',Z),
        response(Z),
        loop.
    


