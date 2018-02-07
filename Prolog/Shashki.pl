use_module(library(pce)).

%--------------------------------------------------------------------------------------------------%
% Display game board
display_board(PL1,PL2,F):- send(@dialog, clear),
                    new(KL, bitmap(image('desk.jpg'))),
                    send(@dialog, display, KL, point(0,0)),
                    display_lines(1,PL1,PL2,F).

% Check player's figure 
check_fig(I,J,[[_,ROW,COL]|_]):-I=COL,J=ROW,!.
check_fig(I,J,[[_,_,_]|Y]):-check_fig(I,J,Y).

% Check all players figures
check_figure(I,J,PL1,_,1):-check_fig(I,J,PL1),
                           new(C, bitmap(pixmap('white.jpg'))),
                           send(C, recogniser, click_gesture(left,'',single, 
                                message(@prolog, add_can_move, J,I))),
                           send(@dialog, display, C, point(128*(J-1)+10 , 128*(I-1)+10)),!.
check_figure(I,J,PL1,_,0):-check_fig(I,J,PL1),
                           new(C, bitmap(pixmap('white.jpg'))),
                           send(@dialog, display, C, point(128*(J-1)+10 , 128*(I-1)+10)),!.
check_figure(I,J,_,PL2,_):-check_fig(I,J,PL2),
                           new(C, bitmap(pixmap('black.jpg'))),
                           send(@dialog, display, C, point(128*(J-1)+10, 128*(I-1)+10)),!.
check_figure(_,_,_,_,_).

% Display horizontal line of board
display_line(I,J,PL1,PL2,F):-J<9,
                       check_figure(I,J,PL1,PL2,F),
                       J1 is J+1,!,display_line(I,J1,PL1,PL2,F).
display_line(_,J,_,_,_):-J=9,!.

% Display all lines of board
display_lines(I,PL1,PL2,F):-I<9,I1 is I+1,display_line(I,1,PL1,PL2,F),display_lines(I1,PL1,PL2,F),!.
display_lines(_,_,_,_).
%--------------------------------------------------------------------------------------------------%

% Set board to database
set_brd(X) :-retract(brd(_,_,_)),asserta(X).
set_brd(X) :-asserta(X).

% Player vs Mashine:
game_play1:-new(@dialog, dialog('Шашки')), 
            send(@dialog,width(1040)), 
            send(@dialog,height(1040)), 
            send(@dialog, open),!,
            game_play1_2().

% Show board
game_play1_2():-brd(b,PL1,PL2),
                display_board(PL1,PL2,1).


% Start function -- begin(game_play1) -- start game player vs computer
begin(Game_play):- set_brd(brd(b,[[p,1,1], [p,3,1], [p,5,1], [p,7,1],
                       [p,2,2], [p,4,2], [p,6,2], [p,8,2],
                       [p,1,3], [p,3,3], [p,5,3], [p,7,3]],
                                                    
                      [[p,2,8], [p,4,8], [p,6,8], [p,8,8],
                       [p,1,7], [p,3,7], [p,5,7], [p,7,7],
                       [p,2,6], [p,4,6], [p,6,6], [p,8,6]]
                    )
               ),
        Game_play,!.
%--------------------------------------------------------------------------------------------------%



% check whether board cell is empty
empty_cell(Row,Col,B):-brd(B,PL1,PL2),!,empty_cell(Row,Col,B,PL1),empty_cell(Row,Col,B,PL2).

empty_cell(Row,Col,B,[[p,R,C]|Y]):- [Row,Col] \= [R,C],!, empty_cell(Row,Col,B,Y).
empty_cell(Row,Col,_,[[p,R,C]|_]):- [Row,Col]  = [R,C],!, fail.
empty_cell(_,_,_,_).

% Check wheter this move is valid
valid_move([R1,C1],[R2,C2]):-R2 is R1+1, C2 is C1+1,R2<9,C2<9,R2>0,C2>0.
valid_move([R1,C1],[R2,C2]):-R2 is R1+1, C2 is C1-1,R2<9,C2<9,R2>0,C2>0.
valid_move([R1,C1],[R2,C2]):-R2 is R1-1, C2 is C1+1,R2<9,C2<9,R2>0,C2>0.
valid_move([R1,C1],[R2,C2]):-R2 is R1-1, C2 is C1-1,R2<9,C2<9,R2>0,C2>0.

% Check wheter this bit is valid
valid_bit(R1,C1,R2,C2,R3,C3):-R2 is R1+2, C2 is C1+2,R3 is R1+1, C3 is C1+1,R2<9,C2<9,R2>0,C2>0.
valid_bit(R1,C1,R2,C2,R3,C3):-R2 is R1+2, C2 is C1-2,R3 is R1+1, C3 is C1-1,R2<9,C2<9,R2>0,C2>0.
valid_bit(R1,C1,R2,C2,R3,C3):-R2 is R1-2, C2 is C1+2,R3 is R1-1, C3 is C1+1,R2<9,C2<9,R2>0,C2>0.
valid_bit(R1,C1,R2,C2,R3,C3):-R2 is R1-2, C2 is C1-2,R3 is R1-1, C3 is C1-1,R2<9,C2<9,R2>0,C2>0.

not(X):-X,!,fail.
not(_).

% Check valid move
check_move([R1,C1],[R2,C2],B, PL1, _):- valid_move([R1,C1],[R2,C2]), 
                                        empty_cell(R2,C2,B), not(empty_cell(R1,C1,B,PL1)).

check_bit([R1,C1],[R2,C2],B,[R3,C3],PL1,PL2):- valid_bit(R1,C1,R2,C2,R3,C3), empty_cell(R2,C2,B),
                                               not(empty_cell(R1,C1,B,PL1)), not(empty_cell(R3,C3,B,PL2)).

%Make move
make_move([R1,C1],[R2,C2],B,PL1,PL2):- check_move([R1,C1],[R2,C2],B,PL1,PL2),
      delete(PL1,[p,R1,C1],PPL1), append(PPL1,[[p,R2,C2]],PNL1),
      retract(brd(B,PL1,PL2)), NB = brd(B,PNL1,PL2), assertz(NB).

%Make bit
make_move([R1,C1],[R2,C2],B,PL1,PL2):- check_bit([R1,C1],[R2,C2],B,[R3,C3],PL1,PL2),
      delete(PL1,[p,R1,C1],PPL1), delete(PL2,[p,R3,C3],PPL2), append(PPL1,[[p,R2,C2]],PNL1),
      retract(brd(B,PL1,PL2)), NB = brd(B,PNL1,PPL2), assertz(NB).


%Player move
make_pl_move([R1,C1],[R2,C2],B):- brd(B,PL1,PL2), 
                                  make_move([R1,C1],[R2,C2],B,PL1,PL2).

%Check end of game
chck_fin([],_,1):- final(2).
chck_fin(_,[],1):- final(1).
chck_fin(_,[],2):- final(2).
chck_fin([],_,2):- final(1).
chck_fin(_,_,_).

%Show end button
final(P):- mess(P,M),
           new(B,button(M,message(@dialog, destroy))),
           send(B, width(200)),
           send(B, height(100)),
           send(@dialog, display, B, point(500,500)),fail.

mess(1, M):- M ='Игрок 1 победил!'.
mess(2, M):- M ='Игрок 2 победил!'.


%Make player's end computer's move
player_move(R1,C1,R2,C2):- make_pl_move([R1,C1],[R2,C2],b),
                           brd(b,PLM,PLL),
                           display_board(PLM, PLL,0),
                           chck_fin(PLM,PLL,1),
                           retract(brd(b,PLM,PLL)),
                           NB = brd(b,PLL,PLM),
                           assertz(NB),
                           comp_move(PLL,PLM),
                           brd(b,PLL1,PLM1),
                           display_board(PLM1, PLL1,1),
                           chck_fin(PLL1,PLM1,2),
                           retract(brd(b,PLL1,PLM1)),
                           NB1 = brd(b,PLM1,PLL1),
                           assertz(NB1).

%Add valid moves for player on desc
add_can_move(R1,C1):- brd(b,PL1,PL2),
                      findall(Y, check_move([R1,C1],Y,b,PL1,PL2), L1),
                      findall(Y, check_bit([R1,C1],Y,b,_,PL1,PL2), L2),
                      append(L1,L2,L),
                      set_buttons([R1,C1],L).


% Set buttons of valid moves
set_buttons([R1,C1],[[R2,C2]|T]):- new(C, bitmap(pixmap('yellow.jpg'))),
    send(C, recogniser, click_gesture(left,'',single, 
        message(@prolog, player_move, R1,C1,R2,C2))),
    send(@dialog, display, C, point(128*(R2-1)+10 , 128*(C2-1)+10)),
    set_buttons([R1,C1],T).
set_buttons(_,[],_,_).


% Computer's move
comp_move(PL1,PL2):-  assertz(move_cost([0,0,0,0,0])),
                      generate_first_move(PL1,PL2,PL1,PL2,4),
                      retract(move_cost([C,R1,C1,R2,C2])),
                      display(['-------------',C,R1,C1,R2,C2]),nl,
                      retractall(move_cost(_)),
                      make_move([R1,C1],[R2,C2],b,PL1,PL2),!.
%--------------------------------------------------------------------------------------------------%
                    


%All valid moves
v_m(B,R1,C1,R2,C2,PL1,_):-R2 is R1+1, C2 is C1+1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)).
v_m(B,R1,C1,R2,C2,PL1,_):-R2 is R1+1, C2 is C1-1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)).
v_m(B,R1,C1,R2,C2,PL1,_):-R2 is R1-1, C2 is C1+1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)).
v_m(B,R1,C1,R2,C2,PL1,_):-R2 is R1-1, C2 is C1-1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)).

%All valid bits
v_b(B,R1,C1,R2,C2,R3,C3,PL1,PL2):-R2 is R1+2, C2 is C1+2,R3 is R1+1, C3 is C1+1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)),not(empty_cell(R3,C3,B,PL2)).
v_b(B,R1,C1,R2,C2,R3,C3,PL1,PL2):-R2 is R1+2, C2 is C1-2,R3 is R1+1, C3 is C1-1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)),not(empty_cell(R3,C3,B,PL2)).
v_b(B,R1,C1,R2,C2,R3,C3,PL1,PL2):-R2 is R1-2, C2 is C1+2,R3 is R1-1, C3 is C1+1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)),not(empty_cell(R3,C3,B,PL2)).
v_b(B,R1,C1,R2,C2,R3,C3,PL1,PL2):-R2 is R1-2, C2 is C1-2,R3 is R1-1, C3 is C1-1,R2<9,C2<9,R2>0,C2>0,empty_cell(R2,C2,B),not(empty_cell(R1,C1,B,PL1)),not(empty_cell(R3,C3,B,PL2)).


generate_first_move([[p,R1,C1]|Y],PL2,PLL1,PLL2,D):- brd(b,PLL1,PLL2),!, 
                                    findall((R1,C1,R2,C2),v_m(b,R1,C1,R2,C2,PLL1,PLL2),VM),
                                    findall((R1,C1,R2,C2,R3,C3),v_b(b,R1,C1,R2,C2,R3,C3,PLL1,PLL2),VB),
                                    % display('--New First Move'),nl,
                                    generate_first_new_moves(VM,VB,PLL1,PLL2,D),
                                    generate_first_move(Y,PL2,PLL1,PLL2,D),
                                    retractall(brd(cmp1,_,_)),retractall(brd(cmp2,_,_)),!.

generate_first_move([],_,_,_,_).

%Generates all possible moves
generate_move(PL1,PL2,_,_,R,0,B,_):- NB =brd(B,PL1,PL2), assertz(NB),
                                     calc_cost(B,PL1,PL2,R),
                                     % display(['Cost = ',R]),nl,
                                     retractall(brd(B,PL1,PL2)).
generate_move([],_,_,_,REZ,_,_,1):- REZ = 0.
generate_move([],_,_,_,REZ,_,_,2):- REZ = 100000.
generate_move([[p,R1,C1]|Y],PL2,PLL1,PLL2,REZ,D,B,1):- 
                                    NB =brd(B,PLL1,PLL2), assertz(NB),
                                    findall((R1,C1,R2,C2),v_m(B,R1,C1,R2,C2,PLL1,PLL2),VM),
                                    findall((R1,C1,R2,C2,R3,C3),v_b(B,R1,C1,R2,C2,R3,C3,PLL1,PLL2),VB),
                                    retractall(brd(cmp2,_,_)), 
                                    % display(['----New Move 1',B]),nl,
                                    generate_new_moves(VM,VB,PLL1,PLL2,D,REZ2,1),
                                    % display('----New Move 1'),nl,
                                    generate_move(Y,PL2,PLL1,PLL2,REZ1,D,B,1),
                                    random(1,3,R), maximum(REZ1,REZ2,REZ,R),!.
generate_move([[p,R1,C1]|Y],PL2,PLL1,PLL2,REZ,D,B,2):- 
                                    NB =brd(B,PLL1,PLL2), assertz(NB),
                                    findall((R1,C1,R2,C2),v_m(B,R1,C1,R2,C2,PLL1,PLL2),VM),
                                    findall((R1,C1,R2,C2,R3,C3),v_b(B,R1,C1,R2,C2,R3,C3,PLL1,PLL2),VB),
                                    retractall(brd(cmp1,_,_)),
                                    % display(['----New Move 2',B,VM,VB,PLL1,PLL2,D]),nl,
                                    generate_new_moves(VM,VB,PLL1,PLL2,D,REZ2,2),
                                    % display('----New Move 2'),nl,
                                    generate_move(Y,PL2,PLL1,PLL2,REZ1,D,B,2),
                                    random(1,3,R), minimum(REZ1,REZ2,REZ,R),!.

% Generate list of moves             
gen_move_lst([[p,R1,C1]|Y],PL2,B,[(VM,VB,[p,R1,C1])|L]):- brd(B,PL1,PL2),                
                                    findall((R1,C1,R2,C2),v_m(B,R1,C1,R2,C2,PL1,PL2),VM),
                                    findall((R1,C1,R2,C2,R3,C3),v_b(B,R1,C1,R2,C2,R3,C3,PL1,PL2),VB),
                                    gen_move_lst(Y,PL2,B,L),!.
gen_move_lst(_,_,_,[]).

% Max of two costs
max([X|X1],[Y|_],Z,_):- X>Y,Z=[X|X1],!.
max([X|X1],[Y|_],Z,R):- X==Y,R==1, Z = [X|X1],!. 
max([X|_],[Y|Y1],Z,R):- X==Y,R==2, Z = [Y|Y1],!. 
max(_,Y,Z,_):- Z = Y,!.

maximum(X,Y,Z,_):- X>Y,Z=X,!.
maximum(X,Y,Z,R):- X==Y,R==1, Z = X,!. 
maximum(X,Y,Z,R):- X==Y,R==2, Z = Y,!. 
maximum(_,Y,Z,_):- Z = Y,!.

minimum(0,Y,Z,_):- Z=Y,!.
minimum(X,0,Z,_):- Z=X,!.
minimum(X,Y,Z,_):- X<Y,Z=X,!.
minimum(X,Y,Z,R):- X==Y,R==1, Z = X,!. 
minimum(X,Y,Z,R):- X==Y,R==2, Z = Y,!. 
minimum(_,Y,Z,_):- Z = Y,!.

%Calculate cost function
calc_cost(cmp1,PL1,PL2,R):-NB =brd(cmp1,PL1,PL2), assertz(NB),
                            gen_move_lst(PL1,PL2,cmp1,L),
                            retractall(brd(cmp1,_,_)),
                            length_move_list(L,R).
calc_cost(cmp2,PL1,PL2,R):-NB =brd(cmp2,PL1,PL2), assertz(NB),
                            gen_move_lst(PL1,PL2,cmp2,L),
                            retractall(brd(cmp2,_,_)),
                            length_move_list(L,R).
                
% Length of moves list
length_move_list([(L1,L2,L3)|Y], N):- length(L1,R1),length(L2,R2),length(L3,R3), 
                                      length_move_list(Y,N1), N is R1+R2+R3+N1+100,!.
length_move_list([], N):- N=0. 

generate_first_new_moves(A,[(R1,C1,R2,C2,_,_)|Y],PL1,PL2,D):-
    NB =brd(cmp1,PL1,PL2), assertz(NB),
    make_move([R1,C1],[R2,C2],cmp1,PL1,PL2), 
    D1 is D-1, brd(cmp1,PLM1,PLM2),!,
    retractall(brd(cmp1,_,_)),
    % display('-----Lower level'),nl,
    generate_move(PLM2,PLM1,PLM2,PLM1,REZ,D1,cmp1,2),
    % display(['-----Come back',A,Y,REZ,D]),nl,
    generate_first_new_moves(A,Y,PL1,PL2,D),
    retract(move_cost(Old_cost)),
    random(1,3,R),REZ1 is REZ+5000,
    max([REZ1,R1,C1,R2,C2],Old_cost,New_cost,R),
    NMV = move_cost(New_cost),
    assertz(NMV),!.
generate_first_new_moves([(R1,C1,R2,C2)|Y],[],PL1,PL2,D):-
    NB =brd(cmp1,PL1,PL2), assertz(NB),
    make_move([R1,C1],[R2,C2],cmp1,PL1,PL2), 
    D1 is D-1, brd(cmp1,PLM1,PLM2),!,
    retractall(brd(cmp1,_,_)),
    % display('-----Lower level'),nl,
    generate_move(PLM2,PLM1,PLM2,PLM1,REZ,D1,cmp1,2),
    % display(['-----Come back',A,Y,REZ,D]),nl,
    generate_first_new_moves(Y,[],PL1,PL2,D),
    retract(move_cost(Old_cost)),
    random(1,3,R),
    max([REZ,R1,C1,R2,C2],Old_cost,New_cost,R),
    NMV = move_cost(New_cost),
    assertz(NMV),!.
generate_first_new_moves([],[],_,_,_).


generate_new_moves(A,[(R1,C1,R2,C2,_,_)|Y],PL1,PL2,D,REZ,1):-
    NB =brd(cmp1,PL1,PL2), assertz(NB),
    % display('make move args '),display([R1,C1,R2,C2,PL1,PL2]),nl,
    make_move([R1,C1],[R2,C2],cmp1,PL1,PL2), 
    D1 is D-1, brd(cmp1,PLM1,PLM2),!,
    retractall(brd(cmp1,_,_)),
    % display('-----Lower level - '),display(D1),nl,
    generate_move(PLM2,PLM1,PLM2,PLM1,REZ1,D1,cmp1,2),
    % display('-----Come from - '),display(D1),nl,
    generate_new_moves(A,Y,PL1,PL2,D,REZ2,1),
    REZ is REZ1+REZ2,!.

generate_new_moves([(R1,C1,R2,C2)|Y],[],PL1,PL2,D,REZ,1):- 
    NB =brd(cmp1,PL1,PL2), assertz(NB),
    % display('make move args '),display([R1,C1,R2,C2,PL1,PL2]),nl,
    make_move([R1,C1],[R2,C2],cmp1,PL1,PL2), 
    D1 is D-1, brd(cmp1,PLM1,PLM2),!,
    retractall(brd(cmp1,_,_)),
    % display('-----Lower level - '),display(D1),nl,
    generate_move(PLM2,PLM1,PLM2,PLM1,REZ1,D1,cmp1,2),
    % display('-----Come from - '),display(D1),nl,
    generate_new_moves(Y,[],PL1,PL2,D,REZ2,1),
    REZ is REZ1+REZ2,!.

generate_new_moves(A,[(R1,C1,R2,C2,_,_)|Y],PL1,PL2,D,REZ,2):-
    NB =brd(cmp2,PL1,PL2), assertz(NB),
    make_move([R1,C1],[R2,C2],cmp2,PL1,PL2), 
    D1 is D-1, brd(cmp2,PLM1,PLM2),!,
    retractall(brd(cmp1,_,_)),
    % display('-----Lower level - '),display(D1),nl,
    generate_move(PLM2,PLM1,PLM2,PLM1,REZ1,D1,cmp2,1),
    % display('-----Come from - '),display(D1),nl,
    generate_new_moves(A,Y,PL1,PL2,D,REZ2,2),
    REZ is REZ1+REZ2+2000,!.

generate_new_moves([(R1,C1,R2,C2)|Y],[],PL1,PL2,D,REZ,2):- 
    NB =brd(cmp2,PL1,PL2), assertz(NB),
    make_move([R1,C1],[R2,C2],cmp2,PL1,PL2), 
    D1 is D-1, brd(cmp2,PLM1,PLM2),!,
    retractall(brd(cmp2,_,_)),
    % display('-----Lower level - '),display(D1),nl,
    generate_move(PLM2,PLM1,PLM2,PLM1,REZ1,D1,cmp2,1),
    % display('-----Come from - '),display(D1),nl,
    generate_new_moves(Y,[],PL1,PL2,D,REZ2,2),
    REZ is REZ1+REZ2,!.

generate_new_moves([],[],_,_,_,R,_):- R=0.