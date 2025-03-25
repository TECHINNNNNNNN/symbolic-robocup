% ========== BASIC DEFINITIONS ==========
field(size(100, 50)).
goal_position(team1, 100, 25).
goal_position(team2, 0, 25).

:- dynamic game_state/3.
:- dynamic ball/2.
:- dynamic player/7.

% ========== INITIALIZATION ==========
reset_game :-
    retractall(game_state(_, _, _)),
    assertz(game_state(0, 0, 0)).

reset_ball :-
    retractall(ball(_, _)),
    assertz(ball(position(50, 25), none)).

initialize_players :-
    % Team 1
    assertz(player(team1, forward, position(10, 30), 100, 3, 70, 30)),
    assertz(player(team1, defender, position(20, 20), 100, 2, 40, 80)),
    assertz(player(team1, goalkeeper, position(0, 25), 100, 1, 20, 90)),
    % Team 2
    assertz(player(team2, forward, position(90, 30), 100, 3, 70, 30)),
    assertz(player(team2, defender, position(80, 20), 100, 2, 40, 80)),
    assertz(player(team2, goalkeeper, position(100, 25), 100, 1, 20, 90)).

% ========== HELPER PREDICATES ==========
abs(X, X) :- X >= 0, !.
abs(X, Y) :- Y is -X.

sign(X, 1) :- X > 0, !.
sign(X, -1) :- X < 0, !.
sign(0, 0).

distance(position(X1,Y1), position(X2,Y2), D) :-
    number(X1), number(Y1), number(X2), number(Y2),
    DX is X2 - X1,
    DY is Y2 - Y1,
    D is sqrt(DX*DX + DY*DY).

in_defensive_zone(Team, position(X, _)) :-
    goal_position(Team, GoalX, _),
    number(X), number(GoalX),
    abs(X - GoalX, Dist),
    Dist =< 20.

opponent_forward_near(Team, OppPos, Distance) :-
    (Team == team1 -> OppTeam = team2 ; OppTeam = team1),
    player(OppTeam, forward, OppPos, _, _, _, _),
    player(Team, defender, MyPos, _, _, _, _),
    distance(MyPos, OppPos, D),
    D =< Distance.

compare_distances(<, [_,D1], [_,D2]) :- D1 < D2.
compare_distances(>, [_,D1], [_,D2]) :- D1 > D2.

% ========== CORE MECHANICS ==========
update_round :-
    game_state(Round, S1, S2),
    NewRound is Round + 1,
    retract(game_state(Round, S1, S2)),
    assertz(game_state(NewRound, S1, S2)).

check_half_time :-
    game_state(30, _, _),
    format('Half time! Resetting stamina.~n'),
    reset_stamina.

reset_stamina :-
    retract(player(T, R, P, _, Sp, Dr, De)),
    assertz(player(T, R, P, 90, Sp, Dr, De)),
    fail.
reset_stamina.

move_towards(_, _, P, P, _, P) :- !.
move_towards(Team, Role, position(X1,Y1), position(X2,Y2), Speed, position(NewX,NewY)) :-
    number(X1), number(Y1), number(X2), number(Y2), number(Speed),
    XDiff is X2 - X1,
    YDiff is Y2 - Y1,
    sign(XDiff, DX),
    sign(YDiff, DY),
    MoveX is DX * min(Speed, abs(XDiff)),
    MoveY is DY * min(Speed, abs(YDiff)),
    NewX is X1 + MoveX,
    NewY is Y1 + MoveY,
    field(size(FW,FH)),
    clamp(NewX, 0, FW, ClampedX),
    clamp(NewY, 0, FH, ClampedY),
    (ClampedX =\= X1 ; ClampedY =\= Y1),
    NewPos = position(ClampedX, ClampedY).

clamp(V, Min, Max, Min) :- V < Min, !.
clamp(V, Min, Max, Max) :- V > Max, !.
clamp(V, _, _, V).

% ========== PLAYER ACTIONS ==========
forward_behavior(Team) :-
    player(Team, forward, Pos, Stamina, Speed, Drib, Def),
    (ball(BallPos, Team) ->
        random(0, 100, Choice),
        (Choice < 30 -> attempt_pass(Team, forward) ;
         Choice < 70 -> dribble(Team, forward) ;
         kick_ball(Team, forward))
    ;
        (ball(BallPos, none) -> move_towards(Team, forward, Pos, BallPos, Speed, NewPos)
        ; (Team == team1 -> goal_position(team2, GX, GY) ; goal_position(team1, GX, GY)),
          move_towards(Team, forward, Pos, position(GX, GY), Speed, NewPos)
        ),
        NewStamina is max(0, Stamina - Speed),
        update_player(Team, forward, Pos, NewPos, NewStamina, Speed, Drib, Def),
        format('~w forward moves to ~w, stamina ~w~n', [Team, NewPos, NewStamina])
    ).

defender_behavior(Team) :-
    player(Team, defender, Pos, Stamina, Speed, Drib, Def),
    (in_defensive_zone(Team, Pos) -> 
        (opponent_forward_near(Team, OppPos, 15) -> 
            move_towards(Team, defender, Pos, OppPos, Speed, NewPos)
        ;
            goal_position(Team, GX, GY),
            sign(GX, S), DefendX is GX + (S * 15),
            move_towards(Team, defender, Pos, position(DefendX, GY), Speed, NewPos)
    ;
        goal_position(Team, GX, GY),
        sign(GX, S), DefendX is GX + (S * 15),
        move_towards(Team, defender, Pos, position(DefendX, GY), Speed, NewPos)
    ),
    NewStamina is max(0, Stamina - Speed),
    update_player(Team, defender, Pos, NewPos, NewStamina, Speed, Drib, Def),
    format('~w defender moves to ~w, stamina ~w~n', [Team, NewPos, NewStamina]).

goalkeeper_behavior(Team) :-
    player(Team, goalkeeper, GkPos, Stamina, Speed, Drib, Def),
    ball(BallPos, Holder),
    ( (Holder \== Team, distance(GkPos, BallPos, D), D =< 20) ->
        random(0, 100, SaveRoll),
        (SaveRoll < Def ->
            move_towards(Team, goalkeeper, GkPos, BallPos, Speed, NewPos),
            (distance(NewPos, BallPos, D2), D2 =< 2 ->
                retract(ball(_, _)),
                assertz(ball(NewPos, Team)),
                format('~w goalkeeper saves!~n', [Team])
            ; true)
        ; true)
    ;
        goal_position(Team, GX, GY),
        move_towards(Team, goalkeeper, GkPos, position(GX, GY), Speed//2, NewPos)
    ),
    NewStamina is max(0, Stamina - Speed//3),
    update_player(Team, goalkeeper, GkPos, NewPos, NewStamina, Speed, Drib, Def).

update_player(Team, Role, OldPos, NewPos, Stamina, Speed, Drib, Def) :-
    retract(player(Team, Role, OldPos, _, Speed, Drib, Def)),
    assertz(player(Team, Role, NewPos, Stamina, Speed, Drib, Def)).

% ========== BALL ACTIONS ==========
gain_possession(Team, Role) :-
    player(Team, Role, Pos, _, _, _, _),
    ball(BallPos, none),
    distance(Pos, BallPos, D),
    D =< 5,
    retract(ball(BallPos, none)),
    assertz(ball(BallPos, Team)),
    format('~w ~w gains possession!~n', [Team, Role]).

dribble(Team, Role) :-
    ball(BallPos, Team),
    player(Team, Role, Pos, Stamina, Speed, Drib, Def),
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    DribbleSpeed is Speed * 0.7,
    move_towards(Team, Role, Pos, position(GoalX, GoalY), DribbleSpeed, NewPos),
    NewStamina is Stamina - DribbleSpeed,
    retract(player(Team, Role, Pos, Stamina, Speed, Drib, Def)),
    assertz(player(Team, Role, NewPos, NewStamina, Speed, Drib, Def)),
    retract(ball(_, Team)),
    assertz(ball(NewPos, Team)),
    NewPos = position(NewX, NewY),
    format('~w ~w dribbles to (~w, ~w), stamina ~w~n', [Team, Role, NewX, NewY, NewStamina]).

attempt_pass(Team, Role) :-
    ball(BallPos, Team),
    player(Team, Role, Pos, Stamina, Speed, Drib, Def),
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    findall(
        [TeammateRole, DistToGoal, TeammatePos],
        (player(Team, TeammateRole, TeammatePos, _, _, _, _),
         TeammateRole \= Role,
         distance(TeammatePos, position(GoalX, GoalY), DistToGoal)),
        Teammates),
    predsort(compare_distances, Teammates, SortedTeammates),
    SortedTeammates = [[BestRole, _, TeammatePos] | _],
    distance(Pos, TeammatePos, PassDist),
    (PassDist =< 30 ->
        retract(ball(_, Team)),
        assertz(ball(TeammatePos, Team)),
        TeammatePos = position(TX, TY),
        format('~w ~w passes to ~w at (~w, ~w)~n', [Team, Role, BestRole, TX, TY])
    ;
        format('~w ~w considers passing but no good options~n', [Team, Role])
    ).

attempt_steal(Team, Role) :-
    ball(BallPos, OppTeam),
    OppTeam \= Team,
    player(Team, Role, Pos, Stamina, Speed, Drib, Def),
    player(OppTeam, forward, OppPos, _, _, OppDrib, _),
    distance(Pos, OppPos, D),
    D =< 10,
    NewStamina is Stamina - 5,
    ( (Def - OppDrib) > 0 -> 
        Success = true
    ; 
        random(0, 100, Chance),
        Chance < 10
    ),
    (Success ->
        retract(ball(_, OppTeam)),
        assertz(ball(Pos, Team)),
        retract(player(Team, Role, Pos, Stamina, Speed, Drib, Def)),
        assertz(player(Team, Role, Pos, NewStamina, Speed, Drib, Def)),
        format('~w ~w steals the ball! Stamina now ~w~n', [Team, Role, NewStamina])
    ;
        retract(player(Team, Role, Pos, Stamina, Speed, Drib, Def)),
        assertz(player(Team, Role, Pos, NewStamina, Speed, Drib, Def)),
        format('~w ~w attempts to steal but fails. Stamina now ~w~n', [Team, Role, NewStamina])
    ).

kick_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed, Drib, Def),
    ball(position(X2, Y2), Team),
    abs(X1 - X2, DX), DX =< 5,
    abs(Y1 - Y2, DY), DY =< 5,
    (Team = team1 -> 
        goal_position(team2, GoalX, GoalY),
        OppGoalkeeper = team2
    ; 
        goal_position(team1, GoalX, GoalY),
        OppGoalkeeper = team1
    ),
    distance(position(X2, Y2), position(GoalX, GoalY), DistToGoal),
    GoalChance is min(90, (Drib * 0.8) - (DistToGoal * 0.5)),
    random(0, 100, Roll),
    (Roll < GoalChance ->
        retract(ball(_, Team)),
        assertz(ball(position(GoalX, GoalY), none)),
        format('~w ~w shoots and scores!~n', [Team, Role]),
        check_goal
    ;
        XDiff is GoalX - X2,
        YDiff is GoalY - Y2,
        sign(XDiff, DirX),
        sign(YDiff, DirY),
        KickPower is 10 + (Drib / 10),
        NewBallX is X2 + (DirX * KickPower),
        NewBallY is Y2 + (DirY * KickPower),
        field(size(FieldWidth, FieldHeight)),
        NewBallX >= 0, NewBallX =< FieldWidth,
        NewBallY >= 0, NewBallY =< FieldHeight,
        retract(ball(position(X2, Y2), Team)),
        assertz(ball(position(NewBallX, NewBallY), none)),
        format('~w ~w shoots but misses! Ball moves to (~w, ~w)~n', [Team, Role, NewBallX, NewBallY])
    ).

% ========== GAME RULES ==========
check_goal :-
    ball(position(X, Y), Holder),
    Holder \= none,
    goal_position(team1, GoalX, GoalY),
    abs(X - GoalX, DistX), DistX =< 2,
    GoalYMin is GoalY - 5,
    GoalYMax is GoalY + 5,
    Y >= GoalYMin, Y =< GoalYMax,
    game_state(Round, S1, S2),
    NewS2 is S2 + 1,
    retract(game_state(Round, S1, S2)),
    assertz(game_state(Round, S1, NewS2)),
    format('GOAL! team2 scores! Current score: team1 ~w - ~w team2~n', [S1, NewS2]),
    reset_ball,
    !.
check_goal :-
    ball(position(X, Y), Holder),
    Holder \= none,
    goal_position(team2, GoalX, GoalY),
    abs(X - GoalX, DistX), DistX =< 2,
    GoalYMin is GoalY - 5,
    GoalYMax is GoalY + 5,
    Y >= GoalYMin, Y =< GoalYMax,
    game_state(Round, S1, S2),
    NewS1 is S1 + 1,
    retract(game_state(Round, S1, S2)),
    assertz(game_state(Round, NewS1, S2)),
    format('GOAL! team1 scores! Current score: team1 ~w - ~w team2~n', [NewS1, S2]),
    reset_ball,
    !.
check_goal.

penalty_shootout :-
    game_state(60, S1, S2),
    S1 =:= S2,
    format('Game tied at ~w-~w! Starting penalty shootout...~n', [S1, S2]),
    random(0, 100, Team1Penalty),
    random(0, 100, Team2Penalty),
    (Team1Penalty > Team2Penalty ->
        format('team1 wins the penalty shootout! Final score: team1 ~w - ~w team2~n', [S1+1, S2])
    ;
    Team2Penalty > Team1Penalty ->
        format('team2 wins the penalty shootout! Final score: team1 ~w - ~w team2~n', [S1, S2+1])
    ;
        format('Penalty shootout is tied! Continuing...~n'),
        penalty_shootout
    ).

% ========== GAME SIMULATION ==========
simulate_round :-
    update_round,
    game_state(Round, S1, S2),
    (Round =< 60 ->
        (Round =:= 30 -> 
            check_half_time,
            format('Second half begins!~n')
        ; true),
        format('--- Round ~w ---~n', [Round]),
        check_goal,
        
        % Team1 actions
        (ball(_, team1) -> 
            (random(0, 100, Choice),
            (Choice < 30 -> 
                attempt_pass(team1, forward),
                (ball(_, team1) -> dribble(team1, forward) ; true
             ;
             Choice < 70 -> 
                dribble(team1, forward)
             ;
                kick_ball(team1, forward)
            )
        ;
            forward_behavior(team1)
        ),
        
        % Team2 actions
        (ball(_, team2) -> 
            (random(0, 100, Choice),
            (Choice < 30 -> 
                attempt_pass(team2, forward),
                (ball(_, team2) -> dribble(team2, forward) ; true
             ;
             Choice < 70 -> 
                dribble(team2, forward)
             ;
                kick_ball(team2, forward)
            )
        ;
            forward_behavior(team2)
        ),
        
        % Defenders and goalkeepers
        defender_behavior(team1),
        defender_behavior(team2),
        goalkeeper_behavior(team1),
        goalkeeper_behavior(team2),
        
        % Attempt steals
        (ball(_, team1) -> 
            attempt_steal(team2, defender),
            (ball(_, team1) -> attempt_steal(team2, forward) ; true
        ;
         ball(_, team2) -> 
            attempt_steal(team1, defender),
            (ball(_, team2) -> attempt_steal(team1, forward) ; true
        ; 
            true
        ),
        
        % Attempt to gain possession if ball is loose
        (ball(_, none) -> 
            (random(0, 100, TeamChance),
            (TeamChance < 50 -> 
                (gain_possession(team1, forward) ; 
                 gain_possession(team1, defender))
            ;
                (gain_possession(team2, forward) ; 
                 gain_possession(team2, defender))
            )
        ; 
            true
        ),
        
        % Display game state
        ball(Pos, Holder),
        (Holder == none -> HolderDesc = 'loose' ; HolderDesc = Holder),
        format('Ball at ~w, held by ~w~n', [Pos, HolderDesc]),
        format('Score: team1 ~w - ~w team2~n~n', [S1, S2]),
        sleep(1)
    ;
        format('Game over! Final score: team1 ~w - ~w team2~n', [S1, S2]),
        (S1 =:= S2 -> 
            penalty_shootout
        ;
            (S1 > S2 -> 
                format('team1 wins!~n')
            ;
                format('team2 wins!~n')
            )
        )
    ).

run_simulation :-
    repeat,
    simulate_round,
    game_state(Round, _, _),
    Round >= 60,
    !,
    format('Simulation complete.~n').

% Main entry point
play :-
    reset_game,
    initialize_players,
    reset_ball,
    format('Starting soccer simulation!~n'),
    format('Teams: 2 forwards, 2 defenders, 1 goalkeeper each~n'),
    format('Game will run for 60 rounds with a halftime at round 30~n~n'),
    sleep(2),
    run_simulation.