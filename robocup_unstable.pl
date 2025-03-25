% Define the soccer field dimensions
field(size(100, 50)).

% Define the goal positions
goal_position(team1, 100, 25).  % Team 1's goal at the right end
goal_position(team2, 0, 25).    % Team 2's goal at the left end

% Game state tracking
:- dynamic game_state/3.
game_state(0, 0, 0). % Round, Team1 Score, Team2 Score

% Reset game state
reset_game :-
    retractall(game_state(_, _, _)),
    assertz(game_state(0, 0, 0)).

% Define the ball's initial position and state
:- dynamic ball/2.
ball(position(50, 25), none).  % Start at the center, not possessed by anyone

% Define player positions and attributes
:- dynamic player/7.
% Format: player(Team, Role, Position, Stamina, Speed, Dribbling, Defending)
player(team1, forward, position(10, 30), 100, 3, 70, 30).  % Team 1 forward
player(team1, defender, position(20, 20), 100, 2, 40, 80).  % Team 1 defender
player(team1, goalkeeper, position(0, 25), 100, 1, 20, 90). % Team 1 goalkeeper

player(team2, forward, position(90, 30), 100, 3, 70, 30).  % Team 2 forward
player(team2, defender, position(80, 20), 100, 2, 40, 80).  % Team 2 defender
player(team2, goalkeeper, position(100, 25), 100, 1, 20, 90). % Team 2 goalkeeper

% Helper function to get absolute value
abs(X, X) :- X >= 0.
abs(X, Y) :- X < 0, Y is -X.

% Sign function for movement
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

% Distance between two positions
distance(position(X1, Y1), position(X2, Y2), D) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    D is sqrt(DX*DX + DY*DY).

% Check if player is in their defensive zone (within 20 units of their goal)
in_defensive_zone(Team, position(X, _Y)) :-
    goal_position(Team, GoalX, _),
    abs(X - GoalX, Dist),
    Dist =< 20.

% Update game round
update_round :-
    game_state(Round, S1, S2),
    NewRound is Round + 1,
    retract(game_state(Round, S1, S2)),
    assertz(game_state(NewRound, S1, S2)).

% Check for half-time
check_half_time :-
    game_state(Round, _, _),
    Round =:= 30,
    format('Half time! Resetting stamina.~n'),
    reset_stamina.

% Reset all players' stamina to 90
reset_stamina :-
    retract(player(Team, Role, Pos, _OldStamina, Speed, Drib, Def)),
    assertz(player(Team, Role, Pos, 90, Speed, Drib, Def)),
    fail.
reset_stamina.

% Move towards position
move_towards(Team, Role, position(X1, Y1), position(X2, Y2), Speed, NewPos) :-
    XDiff is X2 - X1,
    YDiff is Y2 - Y1,
    sign(XDiff, DX),
    sign(YDiff, DY),
    MoveX is DX * Speed,
    MoveY is DY * Speed,
    NewX is X1 + MoveX,
    NewY is Y1 + MoveY,
    field(size(FieldWidth, FieldHeight)),
    NewX >= 0, NewX =< FieldWidth,
    NewY >= 0, NewY =< FieldHeight,
    NewPos = position(NewX, NewY).

% Move towards the ball
% Move towards the ball - fixed version
move_towards_ball(Team, Role) :-
    player(Team, Role, Pos, Stamina, Speed, Drib, Def),
    ball(BallPos, Holder),
    (Holder == none ; Holder \= Team), % Only move if we don't have possession
    move_towards(Team, Role, Pos, BallPos, Speed, NewPos),
    NewStamina is Stamina - Speed,
    retract(player(Team, Role, Pos, Stamina, Speed, Drib, Def)),
    assertz(player(Team, Role, NewPos, NewStamina, Speed, Drib, Def)),
    NewPos = position(NewX, NewY), % Extract coordinates from NewPos
    format('~w ~w moves to (~w, ~w), stamina ~w~n', [Team, Role, NewX, NewY, NewStamina]).

% Defender behavior: stay in defensive zone unless forward is close
defender_behavior(Team) :-
    player(Team, defender, Pos, Stamina, Speed, Drib, Def),
    (in_defensive_zone(Team, Pos) -> 
        (opponent_forward_near(Team, OppPos, 15) -> 
            move_towards(Team, defender, Pos, OppPos, Speed, NewPos),
            NewStamina is Stamina - Speed
        ;
            goal_position(Team, GoalX, GoalY),
            sign(GoalX, GoalSign),
            DefendX is GoalX + (GoalSign * 15),
            DefendPos = position(DefendX, GoalY),
            move_towards(Team, defender, Pos, DefendPos, Speed, NewPos),
            NewStamina is Stamina - (Speed / 2)
        )
    ;
        goal_position(Team, GoalX, GoalY),
        sign(GoalX, GoalSign),
        DefendX is GoalX + (GoalSign * 15),
        DefendPos = position(DefendX, GoalY),
        move_towards(Team, defender, Pos, DefendPos, Speed, NewPos),
        NewStamina is Stamina - Speed
    ),
    retract(player(Team, defender, Pos, Stamina, Speed, Drib, Def)),
    assertz(player(Team, defender, NewPos, NewStamina, Speed, Drib, Def)),
    NewPos = position(NewX, NewY),
    format('~w defender moves to (~w, ~w), stamina ~w~n', [Team, NewX, NewY, NewStamina]).

% Check if opponent forward is near a position within Distance units
opponent_forward_near(Team, OppPos, Distance) :-
    (Team == team1 -> OppTeam = team2 ; OppTeam = team1),
    player(OppTeam, forward, OppPos, _, _, _, _),
    player(Team, defender, MyPos, _, _, _, _),
    distance(MyPos, OppPos, D),
    D =< Distance.

% Attempt to gain possession of the ball
gain_possession(Team, Role) :-
    player(Team, Role, Pos, _, _, _, _),
    ball(BallPos, none),
    distance(Pos, BallPos, D),
    D =< 5, % Close enough to gain possession
    retract(ball(BallPos, none)),
    assertz(ball(BallPos, Team)),
    format('~w ~w gains possession of the ball!~n', [Team, Role]).

% Dribble the ball (move slower while maintaining possession)
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

% Attempt to pass the ball to a teammate
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
    TeammatePos = position(TX, TY), % Extract coordinates
    retract(ball(_, Team)),
    assertz(ball(TeammatePos, Team)),
    format('~w ~w passes to ~w at (~w, ~w)~n', [Team, Role, BestRole, TX, TY]).

% Helper for sorting by distance to goal
compare_distances(<, [_, Dist1], [_, Dist2]) :- Dist1 < Dist2.
compare_distances(>, [_, Dist1], [_, Dist2]) :- Dist1 > Dist2.

% Attempt to steal the ball
attempt_steal(Team, Role) :-
    ball(BallPos, OppTeam),
    OppTeam \= Team,
    player(Team, Role, Pos, Stamina, Speed, Drib, Def),
    player(OppTeam, forward, OppPos, _, _, OppDrib, _),
    distance(Pos, OppPos, D),
    D =< 10,
    ( (Def - OppDrib) > 0 -> 
        Success = true
    ; 
        random(0, 100, Chance),
        Chance < 10
    ),
    (Success ->
        retract(ball(_, OppTeam)),
        assertz(ball(Pos, Team)),
        format('~w ~w steals the ball!~n', [Team, Role])
    ;
        format('~w ~w attempts to steal but fails.~n', [Team, Role])
    ).

% Kick the ball toward the opponent's goal
kick_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed, Drib, Def),
    ball(position(X2, Y2), Team),
    abs(X1 - X2, DX), DX =< 5,
    abs(Y1 - Y2, DY), DY =< 5,
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
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
    NewStamina is Stamina - (KickPower / 2),
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed, Drib, Def)),
    assertz(player(Team, Role, position(X1, Y1), NewStamina, Speed, Drib, Def)),
    format('~w ~w kicks the ball to (~w, ~w)~n', [Team, Role, NewBallX, NewBallY]).

% Goalkeeper behavior
goalkeeper_behavior(Team) :-
    player(Team, goalkeeper, GkPos, Stamina, Speed, Drib, Def),
    ball(BallPos, Holder),
    ( (Holder == none ; Holder \= Team),
      distance(GkPos, BallPos, D),
      D =< 15 ->
        move_towards(Team, goalkeeper, GkPos, BallPos, Speed, NewPos),
        NewStamina is Stamina - Speed,
        (distance(NewPos, BallPos, D2), D2 =< 2 ->
            retract(ball(_, _)),
            assertz(ball(NewPos, Team)),
            format('~w goalkeeper catches the ball!~n', [Team])
        ;
            true
        )
    ;
        goal_position(Team, GoalX, GoalY),
        move_towards(Team, goalkeeper, GkPos, position(GoalX, GoalY), Speed, NewPos),
        NewStamina is Stamina - (Speed / 2)
    ),
    retract(player(Team, goalkeeper, GkPos, Stamina, Speed, Drib, Def)),
    assertz(player(Team, goalkeeper, NewPos, NewStamina, Speed, Drib, Def)).

% Goal detection
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

% Reset the ball to the center
reset_ball :-
    retract(ball(_, _)),
    assertz(ball(position(50, 25), none)),
    format('Ball reset to center.~n').

% Penalty shootout if game is tied after regulation
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

% Simulate one round of the game
simulate_round :-
    update_round,
    game_state(Round, S1, S2),
    (Round =< 60 ->
        (Round =:= 30 -> check_half_time ; true),
        format('Round ~w~n', [Round]),
        check_goal,
        
        % Team1 actions
        (ball(_, team1) -> 
            (random(0, 100, Choice),
            (Choice < 30 -> attempt_pass(team1, forward) ;
             Choice < 70 -> dribble(team1, forward) ;
             kick_ball(team1, forward)))
        ;
            move_towards_ball(team1, forward)
        ),
        
        % Team2 actions
        (ball(_, team2) -> 
            (random(0, 100, Choice),
            (Choice < 30 -> attempt_pass(team2, forward) ;
             Choice < 70 -> dribble(team2, forward) ;
             kick_ball(team2, forward)))
        ;
            move_towards_ball(team2, forward)
        ),
        
        % Defenders and goalkeepers
        defender_behavior(team1),
        defender_behavior(team2),
        goalkeeper_behavior(team1),
        goalkeeper_behavior(team2),
        
        % Attempt steals
        (ball(_, team1) -> attempt_steal(team2, defender) ;
         ball(_, team2) -> attempt_steal(team1, defender) ; true),
        
        % Attempt to gain possession if ball is loose
        (ball(_, none) -> 
            (gain_possession(team1, forward) ; 
             gain_possession(team2, forward) ; 
             gain_possession(team1, defender) ; 
             gain_possession(team2, defender))
        ; true),
        
        ball(Pos, Holder),
        (Holder == none -> HolderDesc = 'loose' ; HolderDesc = Holder),
        format('Ball at ~w, held by ~w~n~n', [Pos, HolderDesc])
    ;
        format('Game over! Final score: team1 ~w - ~w team2~n', [S1, S2]),
        (S1 =:= S2 -> penalty_shootout ; true)
    ).

% Run the full simulation
run_simulation :-
    reset_game,
    retractall(ball(_, _)),
    assertz(ball(position(50, 25), none)),
    format('Starting soccer simulation!~n'),
    repeat,
    simulate_round,
    game_state(Round, _, _),
    Round >= 60,
    !,
    format('Simulation complete.~n').