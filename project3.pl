% Example AI that simply draws cards
example_ai_1(game_state(no_error,_,_,_,_), play(drawcard,empty)).


% Example AI that will only play a card if the count can be made with 1 card.
example_ai_2(game_state(no_error,Count,_,_,PState), play(Play,empty)) :-
	pstate(hand,PState,Hand), example_ai_2_rec(Count,Hand,Play).

example_ai_2_rec(_,[],drawcard).
example_ai_2_rec(Count,[Card|_],[wild(Card,Count)]) :- face(Card, k).
example_ai_2_rec(Count,[Card|_],[Card]) :- value(Card,Count).
example_ai_2_rec(Count,[Card|Hand],Play) :- value(Card,V), V\=Count, example_ai_2_rec(Count,Hand,Play).

% Test Your code
%player1(GameState, Play) :- user_interface(GameState,Play).
%player2(GameState, Play) :- project_ai_1(GameState,Play).

%player1(GameState, Play) :- user_interface(GameState,Play).
%player2(GameState, Play) :- project_ai_2(GameState,Play).

%player1(GameState, Play) :- project_ai_1(GameState,Play).
%player2(GameState, Play) :- project_ai_2(GameState,Play).


player1(GameState, Play) :- example_ai_1(GameState,Play).
player2(GameState, Play) :- example_ai_2(GameState,Play).
