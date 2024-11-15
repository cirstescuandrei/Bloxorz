:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').


%% TODO
% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state([]).



%%%%%%
% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

%% TODO
% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile(State, Pos, [Pos - '+' | State]).



%% TODO
% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(State, _, State).


%% TODO
% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target(State, Pos, [Pos - '$' | State]).



%% TODO
% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile(State, Pos, [Pos - '^' | State]).



%% TODO
% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial(State, Pos, [Pos - 'B' , Pos - '+' | State]).



%% TODO
% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]
get_b_pos(State, BlockPos) :-
    (member(_ - 'b', State) ->
        findall(X, member(X - 'b', State), BlockPos), !
    );
    (member(_ - 'B', State) ->
        findall(X, member(X - 'B', State), [BlockPos | _]), !
    ).


%% TODO
% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
get_bounds([], _, _, _, _).
get_bounds([(X, Y) - _ | Rest], Xmin, Xmax, Ymin, Ymax):-
    get_bounds_helper(Rest, X, X, Y, Y, Xmin, Xmax, Ymin, Ymax).

get_bounds_helper([], Xmin, Xmax, Ymin, Ymax, Xmin, Xmax, Ymin, Ymax).
get_bounds_helper([(X, Y) - _ | Rest], CXmin, CXmax, CYmin, CYmax, Xmin, Xmax, Ymin, Ymax):-
    NXmin is min(X, CXmin),
    NXmax is max(X, CXmax),
    NYmin is min(Y, CYmin),
    NYmax is max(Y, CYmax),
    get_bounds_helper(Rest, NXmin, NXmax, NYmin, NYmax, Xmin, Xmax, Ymin, Ymax).


%% TODO
% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
cell_type(tile, 'b').
cell_type(tile, 'B').
cell_type(target, '$').
cell_type(fragile, '^').
cell_type(tile, '+').
cell_type(oswitch, ('o', _)).
cell_type(xswitch, ('x', _)).

get_cell([], _, _):- false.
get_cell([Pos - R | _], Pos, Type):- cell_type(Type, R).
get_cell([_ | T], Pos, Type):- get_cell(T, Pos, Type).


%% TODO
% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
move(State, Move, NewState):-
    ((member(_ - 'B', State) ->
        % B -> bb
        !, get_b_pos(State, BlockPos),
        select(BlockPos - 'B', State, TempState),
        neighbor(BlockPos, Move, NewPos1),
        neighbor2(BlockPos, Move, NewPos2),
        check_bounds(NewPos1, TempState), !,
        check_bounds(NewPos2, TempState), !,
        (((find_switch(TempState, NewPos1, SW1) ->
            ((find_switch(TempState, NewPos2, SW2) ->
                % SW1 + SW2
                (press_switch(TempState, 'b', SW1, TempState1),
                press_switch(TempState1, 'b', SW2, TempState2),
                NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState2]), !
            );
            (   % SW1
                press_switch(TempState, 'b', SW1, TempState1),
                NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState1], !
            ))
            );
        (find_switch(TempState, NewPos2, SW2) ->
            % SW 2
            (press_switch(TempState, 'b', SW2, TempState1),
            NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState1]), !
        ));
            NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState], !
        )
    );
    (member(_ - 'b', State) ->
        !, get_b_pos(State, [B1, B2]),
        select(B1 - 'b', State, S1),
        select(B2 - 'b', S1, TempState),
        sort_pairs(B1, B2, [Pos1, Pos2]),
        ((block_type_change([B1, B2], Move) ->
            % bb -> B
            (
                (Move = u -> neighbor(Pos1, Move, NewPos));
                (Move = d -> neighbor(Pos2, Move, NewPos));
                (Move = l -> neighbor(Pos1, Move, NewPos));
                (Move = r -> neighbor(Pos2, Move, NewPos))
            ),
            check_bounds_fragile(NewPos, TempState), !,
            ((find_switch(TempState, NewPos, SW) ->
                press_switch(TempState, 'B', SW, TempState1),
                NewState = [NewPos -  'B' | TempState1]
            );
                NewState = [NewPos - 'B' | TempState]
            )
        );
        (   % bb -> bb
            neighbor(Pos1, Move, NewPos1),
            neighbor(Pos2, Move, NewPos2),
            check_bounds(NewPos1, TempState), !,
            check_bounds(NewPos2, TempState), !,
            (((find_switch(TempState, NewPos1, SW1) ->
                ((find_switch(TempState, NewPos2, SW2) ->
                    % SW1 + SW2
                    (press_switch(TempState, 'b', SW1, TempState1),
                    press_switch(TempState1, 'b', SW2, TempState2),
                    NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState2]), !
                );
                (   % SW1
                    press_switch(TempState, 'b', SW1, TempState1),
                    NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState1], !
                ))
            );
            (find_switch(TempState, NewPos2, SW2) ->
                % SW 2
                (press_switch(TempState, 'b', SW2, TempState1),
                NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState1]), !
            ));
                NewState = [NewPos1 - 'b', NewPos2 - 'b' | TempState], !
            )
        ))
    )), !.

% Sorts the two pairs in ascending order
sort_pairs((X, Y), (X1, Y1), [(X, Y), (X1, Y1)]) :- X < X1, !.
sort_pairs((X, Y), (X1, Y1), [(X1, Y1), (X, Y)]) :- X > X1, !.
sort_pairs((X, Y), (X1, Y1), [(X, Y), (X1, Y1)]) :- X =:= X1, Y =< Y1, !.
sort_pairs((X, Y), (X1, Y1), [(X1, Y1), (X, Y)]) :- X =:= X1, Y > Y1, !.

% Returns true if after the move a block will stand up
block_type_change([(X, _), (X, _)], u):- !.
block_type_change([(X, _), (X, _)], d):- !.
block_type_change([(_, Y), (_, Y)], r):- !.
block_type_change([(_, Y), (_, Y)], l):- !.

% Checks if move is valid
check_bounds(Pos, State):- member(Pos - _, State), !.
% Checks if move is valid and not a fragile block
% For when the block ends upright
check_bounds_fragile(Pos, State):-
    member(Pos - Type, State), !,
    Type \= '^', !.

%Removes tiles in List from State
remove_pos(_, [], []).
remove_pos([], S, S).
remove_pos([H | T], S, NS):-
    select(H - '+', S, Temp),
    remove_pos(T, Temp, NS).
remove_pos([_ | T], S, NS):-
    remove_pos(T, S, NS).

%Adds tiles from List to State
add_pos([], S, S).
add_pos([H | T], S, [H - '+' | NS]):-
    add_pos(T, S, NS).

%Finds switch at Pos in State
%find_switch(State, Pos, Switch)
find_switch([], _, _):- false.
find_switch([Pos - (Type, Func, Positions) | _], Pos, Pos - (Type, Func, Positions)).
find_switch([_ | T], Pos, Switch):-
    find_switch(T, Pos, Switch).

%press_switch(S, BlockType, Switch, NS)
%Horizontal block lands on heavy switch
press_switch(State, 'b', _ - ('x', _), State):- !.
press_switch(State, _, _ - (_, uponly, [First | Rest]), NewState):-
    (member(First - '+', State) -> 
        NewState = State, !
    );
    (
        add_pos([First | Rest], State, NewState)
    ), !.
press_switch(State, _, _ - (_, dnonly, [First | Rest]), NewState):-
    (member(First - '+', State) ->
        remove_pos([First | Rest], State, NewState), !
    );
    (
        NewState = State
    ), !.
press_switch(State, _, _ - (_, switch, [First | Rest]), NewState):-
    (member(First - '+', State) ->
        remove_pos([First | Rest], State, NewState), !
    );
    (
        add_pos([First | Rest], State, NewState)
    ), !.
press_switch(State, _, _, State).

%% TODO
% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(State) :-
    get_b_pos(State, Pos), !,
    findall(X, member(X - '$', State), [End | _]), !,
    Pos = End.


%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch(State, Pos, oswitch, Func, Positions, [Pos - ('o', Func, Positions) | State]).
set_switch(State, Pos, xswitch, Func, Positions, [Pos - ('x', Func, Positions) | State]).

%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie să ducă blocul în
% picioare pe poziția scop (target).
solve(State, []):-
    is_final(State).

solve(State, Moves):-
    solve(State, Moves, []).

solve(State, Moves, Visited):-
    is_final(State);
    (
        member(Move, [u, d, l, r]),
        move(State, Move, NewState),
        \+ member(NewState, Visited),
        solve(NewState, Rest, [NewState | Visited]),
        Moves = [Move | Rest]
    ).



