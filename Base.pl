:- use_module(library(readutil)).
:- use_module(library(random)).

card_face(a).
card_face(2).
card_face(3).
card_face(4).
card_face(5).
card_face(6).
card_face(7).
card_face(8).
card_face(9).
card_face(10).
card_face(j).
card_face(q).
card_face(k).
card_suit('♠').
card_suit('♥').
card_suit('♦').
card_suit('♣').


card_deck(D) :- findall(X,card(X),D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Config 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use_transcript(true).
use_demo(false).

hand_size(7).
play_to(200).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
card(card(S,F)) :- card_suit(S), card_face(F). % Check if argument is a card
suit(card(S,_),S).
face(card(_,F),F).

same_face(card(_,F), card(_,F)).

% Predicate to check the value of a card for making the count
value(card(_,a), 1) :- !.
value(card(_,j), 10) :- !.
value(card(_,q), 10) :- !.
value(card(_,k), wild) :- !.
value(card(_,F), F).

% Predicate to check the value of a card when scoring
pvalue(card(_,10), 10) :- !.
pvalue(card(_,j), 10) :- !.
pvalue(card(_,q), 10) :- !.
pvalue(card(_,k), 20) :- !.
pvalue(card(_,_), 5).

can_pair(card(_,10)).
can_pair(card(_,j)).
can_pair(card(_,q)).

play_value(card(S,F),V) :- value(card(S,F), V).
play_value(pair(C1,C2), 10) :- can_pair(C1), same_face(C1,C2).
play_value(wild(C,V), V) :- face(C,k).

play_to_cards([],[]).
play_to_cards([pair(C1,C2)|T],[C1,C2|T2]) :- play_to_cards(T,T2).
play_to_cards([wild(C,_)|T], [C|T2]) :- play_to_cards(T,T2).
play_to_cards([C|T],[C|T2]) :- card(C), play_to_cards(T,T2).

% Like the combinatins funtion from scheme.
combinations(List,Combos) :- findall(C,sublist(C,List),Combos).
% You can ignore this part, it is used by combinations/2
sublist([],_).
sublist([H|T],[H|T2]) :- sublist(T,T2).
sublist([H|T],[_|T2]) :- sublist([H|T],T2).

% A Player state is a structure with the following arguments in order
%   - Penalty score
%   - Hand
%   - Custom State

% Get Value from Player State
pstate(score,pstate(S,_,_),S).
pstate(hand,pstate(_,H,_),H).
pstate(custom,pstate(_,_,S),S).
% Get list of Values from Player State
pstate([X],State,[Value]) :- pstate(X,State,Value).
pstate([H|T],State,[Value|VTail]) :- pstate(H,State,Value),pstate(T,State,VTail).

%Set Value in Player State
pstate_set(score,pstate(_,H,C),S, pstate(S,H,C)).
pstate_set(hand,pstate(S,_,C),H, pstate(S,H,C)).
pstate_set(custom,pstate(S,H,_),C, pstate(S,H,C)).
% Set list of Values in Player State
pstate_set([X],StateIn,[Value],StateOut) :- pstate_set(X,StateIn,Value,StateOut).
pstate_set([H|T],StateIn,[Value|VTail],StateOut) :- pstate_set(H,StateIn,Value,State2),pstate_set(T,State2,VTail,StateOut).

% Debug predicate prints out a debug message
debug(Msg) :- write("DEBUG: "),write(Msg),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Driver Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take(N,List,Take) :- take(N,List,Take,_).

take(0, List, [], List).
take(N, [H|T], [H|T2], List) :- N > 0, N2 is N-1, take(N2,T,T2,List).

% State
state_default(state(1,[],[],pstate(0,[],empty),pstate(0,[],empty))).

state(count,state(Count,_,_,_,_), Count).
state(deck,state(_,Deck,_,_,_), Deck).
state(lastplay,state(_,_,LastPlay,_,_), LastPlay).
state(player(1),state(_,_,_,PState,_), PState).
state(player(2),state(_,_,_,_,PState), PState).
state(player(Player,Field), State, Value) :- state(player(Player),State,PState),pstate(Field,PState,Value).
state([X],State,[Value]) :- state(X,State,Value).
state([H|T],State,[Value|T2]) :- state(H,State,Value),state(T,State,T2).

state_set(count,state(_,Deck,LastPlay,PState1,PState2), Count, state(Count,Deck,LastPlay,PState1,PState2)).
state_set(deck,state(Count,_,LastPlay,PState1,PState2), Deck, state(Count,Deck,LastPlay,PState1,PState2)).
state_set(lastplay,state(Count,Deck,_,PState1,PState2), LastPlay, state(Count,Deck,LastPlay,PState1,PState2)).
state_set(player(1),state(Count,Deck,LastPlay,_,PState2), PState1, state(Count,Deck,LastPlay,PState1,PState2)).
state_set(player(2),state(Count,Deck,LastPlay,PState1,_), PState2, state(Count,Deck,LastPlay,PState1,PState2)).
state_set(player(Player,Field), StateIn, Value, StateOut) :- 
	state(player(Player),StateIn,PState),
	pstate_set(Field,PState,Value,PState2),
	state_set(player(Player),StateIn,PState2,StateOut).
state_set([X],StateIn,[Value],StateOut) :- state_set(X,StateIn,Value,StateOut).
state_set([H|T],StateIn,[Value|VTail],StateOut) :- state_set(H,StateIn,Value,State2), state_set(T,State2,VTail,StateOut).

% Game Code

deal_hand(Deck,Hand1,Hand2) :- card_deck(D), random_permutation(D,Shuffled), take(7,Shuffled,Hand1,S2), take(7,S2,Hand2,Deck).

next_turn(1,2).
next_turn(2,1).

% requires predicates player1/6, player2/6 are defined.
playgame :- \+ current_predicate(player1/2), !, write("Player 1 (player1/2) not defined."),nl,fail.
playgame :- \+ current_predicate(player2/2), !, write("Player 2 (player2/2) not defined."),nl,fail.
playgame :- state_default(State), playgame_rec(1,State).

% wrappers around player1/2 and player2/2
player(1,GameState, Play) :- player1(GameState, Play).
player(2,GameState, Play) :- player2(GameState, Play).

playgame_rec(_,State) :- gameover(State),!,do_gameover(State).
playgame_rec(Turn,State) :-
	transcript(["New Hand",nl]),
	demo(newhand),
	new_hand(State,State2),
	play_hand(Turn,State2,State3),!,
	score_hand(State3,State4),
	next_turn(Turn,NextTurn),
	playgame_rec(NextTurn,State4).

new_hand(State,NewState) :-
	deal_hand(Deck,Hand1,Hand2),
	state_set([count,deck,player(1,hand),player(2,hand)],State,[1,Deck,Hand1,Hand2],NewState).

play_hand(_,State,State) :- handover(State),!.
play_hand(Turn,State,NewState) :- 
	do_play_hand(no_error,Turn,State,State2),
	next_turn(Turn,Next),
    play_hand(Next,State2,NewState).

handover(State) :- state(player(1,hand),State,[]),!.
handover(State) :- state(player(2,hand),State,[]),!.
handover(State) :- state(count,State,21).

score_hand(StateIn,StateOut) :-
	score_hand(1,StateIn,State2),
	score_hand(2,State2,StateOut).

score_hand(Player,StateIn,StateOut) :-
	state(player(Player,[score,hand]), StateIn, [Score,Hand]),
	hand_pvalue(Hand,0,Score2), Score3 is Score + Score2,
	state_set(player(Player,score), StateIn, Score3, StateOut).

hand_pvalue([],Acc,Acc).
hand_pvalue([H|T],Acc,Total) :- pvalue(H,PVal), Acc2 is Acc + PVal, hand_pvalue(T,Acc2,Total).

do_play_hand(Err,Turn,State,NewState) :-
	next_turn(Turn,Other),
	state([player(Turn),lastplay,count,player(Other,score)],State,[PState,LastPlay,Count,OScore]),
    player(Turn,game_state(Err,Count,LastPlay,OScore, PState), Play),!,
	process_play(Turn,State,Play,NewState).
do_play_hand(_,Turn,_,_) :-
	nl,write("Player "), write(Turn), write(" will not make a legal move."), fail.

gameover(State) :- state(player(1,score),State,Score), Score >= 200,!.
gameover(State) :- state(player(2,score),State,Score), Score >= 200.

do_gameover(State) :- 
	state([player(1,score),player(2,score)], State, [Score1, Score2]),
	nl,
	write("Player 1's Score: "), write(Score1),nl,
	write("Player 2's Score: "), write(Score2),nl,
	declare_winner(Score1,Score2).

declare_winner(Score1,_) :- Score1 < 200,!,write("Player 1 Wins!").
declare_winner(_,Score2) :- Score2 < 200,!,write("Player 2 Wins!").
declare_winner(Score1,Score2) :- Score1 < Score2,!,write("Player 1 Wins!").
declare_winner(Score1,Score2) :- Score1 > Score2,!,write("Player 2 Wins!").
declare_winner(_,_) :- write("A Tie?").

process_play(Turn,State,Play,NewState) :-
	valid_play(Turn,State,Play),!,
	log_play(Turn,State,Play),
	update_state_by_play(Turn,State,Play,NewState).
process_play(Turn,State,_,NewState) :- do_play_hand(error,Turn,State,NewState).

log_play(Turn,State,Play) :-
	state([count,player(Turn, [score,hand])], State, [Count,[Score,Hand]]),
	transcript([Turn,'. [',Count,']', Play, ' (Hand: ', Hand, ')', nl]),
	demo(turn(Turn,Count,Score,Play,Hand)).

valid_play(_,State,play(drawcard,_)) :- state(deck,State,[]),fail.
valid_play(_,State,play(drawcard,_)) :- !,state(deck,State,Deck), Deck \= [].
valid_play(Turn,State,play(Play,_)) :-
	state([count,player(Turn,hand)], State, [Count,Hand]),
	check_value(Play,Count),
	play_to_cards(Play,Cards),
	check_cards(Cards),
	in_hand(Cards,Hand).

check_value(Cards,Value) :- check_value(Cards,0,Value).
check_value([],Acc,Acc).
check_value([H|T],Acc,Value) :-
	play_value(H,V), Acc2 is Acc+V,
	check_value(T,Acc2,Value).

check_cards([]).
check_cards([H|T]) :-
	\+ member(H,T),
	check_cards(T).

in_hand([],_).
in_hand([H|T],Hand) :- member(H,Hand), in_hand(T,Hand).
	
update_state_by_play(Turn,State,play(drawcard,Custom),NewState) :- 
	state([deck,player(Turn,hand)], State, [[C|Deck],Hand]),
	state_set([deck,lastplay,player(Turn,[hand,custom])],State,[Deck,drawcard,[[C|Hand],Custom]],NewState).
update_state_by_play(Turn,State,play(Play,Custom),NewState) :- 
	Play \= drawcard,
	state([count,player(Turn,hand)], State, [Count,Hand]),
	play_to_cards(Play,Cards),
	remove_from_hand(Cards,Hand,Hand2),
	Count2 is Count+1,
	state_set([count,lastplay,player(Turn,[hand,custom])],State,[Count2,Play,[Hand2,Custom]],NewState).

remove_from_hand([],Hand,Hand).
remove_from_hand([H|T],Hand,NewHand) :-
	remove_card_from_hand(H,Hand,Hand2),
	remove_from_hand(T,Hand2,NewHand).

remove_card_from_hand(_,[],[]).
remove_card_from_hand(Card,[Card|NewHand],NewHand).
remove_card_from_hand(Card,[Card2|Hand],[Card2|NewHand]) :-
	Card \= Card2, remove_card_from_hand(Card,Hand,NewHand).

transcript_object(nl) :- !, nl.
transcript_object(play(Play,_)) :- !,print_plays(Play).
transcript_object(card(S,F)) :- !,print_card(card(S,F)).
transcript_object([H|T]) :- !, write('['), transcript_list([H|T]), write(']').
transcript_object(Msg) :- write(Msg).

print_plays(drawcard) :- write("Draw Card"), !.
print_plays([Play]) :- print_play(Play), !.
print_plays([Play|More]) :- print_play(Play), write(", "),print_opp_move(More).

transcript(Msgs) :- use_transcript(true), dotranscript(Msgs).
transcript(_) :- use_transcript(false).

transcript_list([]).
transcript_list([H]) :- transcript_object(H).
transcript_list([H,H2|T]) :- transcript_object(H), write(','), transcript_list([H2|T]).

dotranscript([]).
dotranscript([H|T]) :- transcript_object(H), dotranscript(T).

demo(Msg) :- use_demo(true), write_canonical(Msg),write("."),nl.
demo(_) :- use_demo(false).

% User interface


user_interface(game_state(error,_,_,_,_),_) :- nl,write("!! Invalid Move !!"),nl,fail.
user_interface(game_state(_,Count,LastPlay,OtherScore,PState), Play) :-
	player_number(LastPlay,PState,N), nl,
	write("================================================================================"), nl,
	write("Player "), write(N),write(" it is your turn"),nl,
	write("================================================================================"), nl,
	pstate_set(custom,PState,N,PState2),
    do_user_interface([], Count, LastPlay, OtherScore, PState2, Play).

player_number([],PState, 1) :- pstate(custom,PState,empty).
player_number(drawcard,PState, 2) :- pstate(custom,PState,empty).
player_number([_|_],PState, 2) :- pstate(custom,PState,empty).
player_number(_,PState, N) :- pstate(custom,PState,N), N \= empty.

do_user_interface(Acc, Count, LastPlay, OtherScore, PState, Play) :- 
    do_user_interface_menu(Acc, Count, LastPlay, OtherScore, PState, Option),
	Option \= invalid,!,
	handle_move(Option, Acc, Count, LastPlay, OtherScore, PState, Play).

handle_move(done, Acc,_,_,_,PState,play(Acc,N)) :- pstate(custom,PState,N).
handle_move(clear, _,Count,LastPlay,OtherScore,PState,Play) :- 
    do_user_interface([], Count, LastPlay, OtherScore, PState, Play).
handle_move(playpair, Acc, Count, LastPlay, OtherScore, PState, FullPlay) :- 
	pstate(hand,PState,Hand), select_pair(Hand,Play),
    handle_play(playpair,Play,Acc, Count, LastPlay, OtherScore, PState, FullPlay).
handle_move(playcard, Acc,Count, LastPlay, OtherScore, PState, FullPlay) :- 
	pstate(hand,PState,Hand), select_card(Hand,Play),
    handle_play(playcard,Play,Acc, Count, LastPlay, OtherScore, PState, FullPlay).
handle_move(drawcard, _,_,_,_,PState,play(drawcard,N)) :- pstate(custom,PState,N).

handle_play(_,back,Acc, Count, LastPlay, OtherScore, PState, FullPlay) :- !,
    do_user_interface(Acc, Count, LastPlay, OtherScore, PState, FullPlay).
handle_play(Option,invalid,Acc, Count, LastPlay, OtherScore, PState, FullPlay) :-
	nl,write("!! Invalid Option !!"),nl,nl,
	handle_move(Option,Acc,Count,LastPlay,OtherScore,PState,FullPlay).
handle_play(_,Play,Acc, Count, LastPlay, OtherScore, PState, FullPlay) :-
    play_to_cards([Play],Cards),
    pstate(hand,PState,Hand),
    remove_from_hand(Cards,Hand,Hand2),
	pstate_set(hand,PState,Hand2,PState2),
    do_user_interface([Play|Acc], Count, LastPlay, OtherScore, PState2, FullPlay).

get_pairs([],[]).
get_pairs([H|T],[pair(H,C2)|T2]) :- 
	can_pair(H),
	find_pair(T,H,C2),!,
	get_pairs(T,T2).
get_pairs([_|T],P) :- get_pairs(T,P).

find_pair([C2|_],C,C2) :- same_face(C,C2),!.
find_pair([_|T],C,C2) :- find_pair(T,C,C2).

select_pair(Hand,Pair) :-
	get_pairs(Hand,Pairs), Pairs \= [],
	create_pair_options(Pairs,Options),
	menu(Options,Pair).
select_pair(_,back) :-
	nl,write("!! You have no valid pairs !!"),nl.
select_card(Hand,Play) :- 
	write("Select Card."),nl,
	create_card_options(Hand,Options),
	menu(Options,Card),
	play_from_card(Card,Play).

play_from_card(C,wild(C,V)) :-
	card(C),face(C,k),!,
	get_wild_value(V).
play_from_card(C,C).

get_wild_value(V) :- prompt1("Enter Wild Card Value (1-20): "), 
                     current_input(Stream),read_line_to_codes(Stream,Line),
					 check_line(Line), number_codes(V, Line), 0 < V, 20 >= V, !.
get_wild_value(V) :-
	write("Invalid Value"),nl,
	get_wild_value(V).

create_pair_options([],[("Back.", back)]).
create_pair_options([H|T], [(HString,H)|T2]) :- pair_to_string(H,HString), create_pair_options(T,T2).

create_card_options([],[("Back.", back)]).
create_card_options([H|T], [(HString,H)|T2]) :- card_to_string(H,HString), create_card_options(T,T2).

fix_face(a,'A') :- !.
fix_face(j,'J') :- !.
fix_face(q,'Q') :- !.
fix_face(k,'K') :- !.
fix_face(F,F).

pair_to_string(pair(card(S1,F1),card(S2,F2)), String) :- fix_face(F1,F3), fix_face(F2,F4),atomics_to_string(['(',S1,F3,',',S2,F4,')'],String).

card_to_string(card(S,F), String) :- fix_face(F,F2), atomics_to_string([S,F2],String).

do_user_interface_menu(Acc, Count, LastPlay, OtherScore, PState,Option) :-
	print_user_information(Acc, Count, LastPlay, OtherScore, PState),
	menu([ ("Draw Card and End Turn", drawcard),
           ("Play Card", playcard),
           ("Play Pair", playpair),
           ("Clear Play", clear),
           ("Done", done)
         ], Option), Option \= invalid.
do_user_interface_menu(Acc, Count, LastPlay, OtherScore, PState, Option) :-
	nl,write("!! Invalid Option !!"),nl,nl,
    do_user_interface_menu(Acc, Count, LastPlay, OtherScore, PState, Option).

print_user_information(Acc, Count, LastPlay, OtherScore, PState) :-
	pstate([score,hand],PState,[Score,Hand]),
	nl,
	write("Your Score: "), write(Score), nl,
	write("Opponent's Score: "), write(OtherScore), nl,
	write("Count: "), write(Count), nl,
	write("Opponent's Move: "), print_opp_move(LastPlay), nl,
	write("Hand: "), print_cards(Hand), nl,
	write("Current Play: "), print_cur_play(Acc),nl,nl.

print_opp_move([]) :- write("You are first!").
print_opp_move(drawcard) :- write("Draw Card").
print_opp_move([Play|More]) :- print_plays([Play|More]).

print_play(drawcard) :- write("Draw Card").
print_play(card(S,F)) :- write(S),fix_face(F,F2),write(F2).
print_play(pair(C1,C2)) :- write("("), print_play(C1), write(","), print_play(C2), write(")").
print_play(wild(C,V)) :- write("("), print_play(C), write(" as "), write(V), write(")").

print_cur_play([]).
print_cur_play([P]) :- print_play(P).
print_cur_play([H|T]) :- print_play(H),write(", "),print_cur_play(T).

print_card(card(S,F)) :- write(S),fix_face(F,F2),write(F2).

print_cards([]).
print_cards([C]) :- print_card(C), !.
print_cards([C|CS]) :- print_card(C), write(", "),print_cards(CS).

menu(Options, Selected) :-
	print_menu(Options), nl,
	prompt1("Select Option: "),
	length(Options, N),
	get_input(Number, N),
	get_option(Options, Number, Selected).

get_option(_,invalid,invalid) :- !.
get_option([(_,Selected)|_], 1, Selected) :- !.
get_option([_|T], N, Selected) :- N>1,N2 is N-1,get_option(T,N2,Selected).

get_input(Selected, N) :- current_input(Stream),read_line_to_codes(Stream,Line), check_line(Line), number_codes(Selected, Line), 0 < Selected, N >= Selected, !.
get_input(invalid,_).

check_line([X]) :- code_type(X,digit), !.
check_line([X|Xs]) :- code_type(X,digit), check_line(Xs).

print_menu(L) :- print_menu(L,1).
print_menu([],_).
print_menu([(Text,_)|Options],N) :- write(N), write(". "), write(Text),nl, N2 is N+1, print_menu(Options,N2).

:- ['project3.lp'].
