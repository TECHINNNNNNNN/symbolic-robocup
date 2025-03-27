% Define the soccer field dimensions
field(size(100, 50)).

% Define the goal positions
goal_position(team1, 100, 25).  % Team 1's goal at the right end
goal_position(team2, 0, 25).    % Team 2's goal at the left end

% Declare all dynamic predicates first
:- dynamic ball/1.
:- dynamic player/7.
:- dynamic game_state/2.
:- dynamic rested_last_round/2.  % Track if a player rested last round

% Sign function for movement
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

% Calculate Euclidean distance between two points
distance(X1, Y1, X2, Y2, Distance) :-
    Distance is sqrt((X2 - X1)^2 + (Y2 - Y1)^2).

% Get the distance from a player to the opponent's goal
distance_to_opponent_goal(Team, Role, Distance) :-
    player(Team, Role, position(X, Y), _, _, _, _),
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    distance(X, Y, GoalX, GoalY, Distance).

% Check if a player is the closest to the opponent's goal among their team
is_closest_to_goal(Team, Role) :-
    distance_to_opponent_goal(Team, Role, Distance),
    \+ (player(Team, OtherRole, _, _, _, _, _),
        Role \= OtherRole,
        distance_to_opponent_goal(Team, OtherRole, OtherDistance),
        OtherDistance < Distance).

% Determine if a player should dribble (speed 1) when closest to goal
should_dribble(Team, Role, AdjustedSpeed) :-
    player(Team, Role, _, _, Speed, _, _),
    (is_closest_to_goal(Team, Role) ->
        AdjustedSpeed is 1,
        format('~w ~w is dribbling (speed reduced to 1)~n', [Team, Role])
    ;   AdjustedSpeed is Speed).

% Move towards the ball with adjusted speed for dribbling
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending),
    ball(position(X2, Y2)),
    (Role = defender -> 
        (goal_position(Team, GoalX, _),
         abs(X1 - GoalX) =< 20) ->  % Defender stays within 20 units of goal
            (abs(X2 - X1) =< 15 ->  % If ball is within 15 units, move toward it
                XDiff is X2 - X1, YDiff is Y2 - Y1
            ;
                XDiff is 0, YDiff is 0)  % Otherwise stay in position
        ;
        XDiff is X2 - X1, YDiff is Y2 - Y1
    ),
    sign(XDiff, DX), sign(YDiff, DY),
    should_dribble(Team, Role, AdjustedSpeed),  % Adjust speed for dribbling
    NewX is X1 + (DX * AdjustedSpeed),
    NewY is Y1 + (DY * AdjustedSpeed),
    field(size(FieldWidth, FieldHeight)),
    NewStamina is Stamina - AdjustedSpeed,
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending)),
    assertz(player(Team, Role, position(NewX, NewY), NewStamina, Speed, Dribbling, Defending)).

% Kick the ball toward the opponent's goal (for field players)
kick_ball(Team, Role) :-
    Role \= goalkeeper,
    \+ rested_last_round(Team, Role),  % Player can't kick if they rested last round
    player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending),
    ball(position(X2, Y2)),
    abs(X1 - X2) =< 5, abs(Y1 - Y2) =< 5,
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    XDiff is GoalX - X2, YDiff is GoalY - Y2,
    sign(XDiff, DX), sign(YDiff, DY),
    KickPower is 5 + (Dribbling // 20),
    NewBallX is X2 + (DX * KickPower),
    NewBallY is Y2 + (DY * KickPower),
    field(size(FieldWidth, FieldHeight)),
    NewBallX >= 0, NewBallX =< FieldWidth,
    NewBallY >= 0, NewBallY =< FieldHeight,
    retract(ball(position(X2, Y2))),
    assertz(ball(position(NewBallX, NewBallY))),
    format('~w ~w kicks the ball to (~w, ~w)~n', [Team, Role, NewBallX, NewBallY]).

% Goalkeeper-specific kick (long powerful kick)
goalkeeper_kick(Team) :-
    player(Team, goalkeeper, position(X1, Y1), Stamina, Speed, Dribbling, Defending),
    ball(position(X2, Y2)),
    abs(X1 - X2) =< 2, abs(Y1 - Y2) =< 2,
    % Goalkeepers kick toward the center line or to a teammate
    (random(3) =:= 0 ->  % 1/3 chance of kicking to center line
        NewBallX is 50,
        NewBallY is 25,
        format('~w goalkeeper makes a long goal kick to center!~n', [Team])
    ;
        % Otherwise kick to a teammate (simplified - just kick forward)
        (Team = team1 -> 
            TargetX is X1 + 40,
            format('~w goalkeeper distributes the ball forward~n', [Team])
        ;
            TargetX is X1 - 40,
            format('~w goalkeeper distributes the ball forward~n', [Team])),
        % Keep Y coordinate within reasonable range
        NewBallY is max(5, min(45, Y1 + random(21) - 10)),
        NewBallX is TargetX
    ),
    field(size(FieldWidth, FieldHeight)),
    NewBallX >= 0, NewBallX =< FieldWidth,
    NewBallY >= 0, NewBallY =< FieldHeight,
    retract(ball(position(X2, Y2))),
    assertz(ball(position(NewBallX, NewBallY))).

% Goalkeeper actions (catch or kick)
goalkeeper_actions(Team) :-
    player(Team, goalkeeper, position(X, Y), Stamina, Speed, Dribbling, Defending), 
    ball(position(BX, BY)),
    abs(X - BX) =< 2, abs(Y - BY) =< 2,
    % Decide whether to catch or kick (50/50 chance)
    (random(2) =:= 0 ->
        catch_ball(Team)
    ;
        goalkeeper_kick(Team)
    ).

% Goalkeeper catches the ball
catch_ball(Team) :-
    player(Team, goalkeeper, position(X, Y), Stamina, Speed, Dribbling, Defending), 
    ball(position(BX, BY)),
    abs(X - BX) =< 2, abs(Y - BY) =< 2,
    retract(ball(position(BX, BY))),
    % Position the ball slightly in front of the goalkeeper (1 unit away)
    (Team = team1 -> NewBallX is X + 1 ; NewBallX is X - 1),
    assertz(ball(position(NewBallX, Y))),
    format('~w goalkeeper catches the ball and positions it at (~w, ~w)!~n', [Team, NewBallX, Y]).

% Goal detection
goal_scored(Team) :-
    ball(position(X, Y)),
    goal_position(OtherTeam, GoalX, GoalY),
    Team \= OtherTeam,
    % Check if the ball is actually in the goal (not just near it)
    (OtherTeam = team1 -> 
        X =< 2  % Team1's goal is at X=0, so ball X should be <= 2
    ; 
        X >= 98  % Team2's goal is at X=100, so ball X should be >= 98
    ),
    GoalYMin is GoalY - 5,
    GoalYMax is GoalY + 5,
    Y >= GoalYMin, Y =< GoalYMax,
    % Additional check to make sure no goalkeeper is holding the ball
    \+ (
        player(OtherTeam, goalkeeper, position(GKX, GKY), _, _, _, _),
        abs(X - GKX) =< 2, 
        abs(Y - GKY) =< 2
    ),
    game_state(score(Team, CurrentScore)),
    NewScore is CurrentScore + 1,
    retract(game_state(score(Team, CurrentScore))),
    assertz(game_state(score(Team, NewScore))),
    game_state(score(team1, Score1)),
    game_state(score(team2, Score2)),
    format('GOAL! ~w scores! Current score: ~w ~w - ~w ~w~n', 
           [Team, team1, Score1, team2, Score2]),
    reset_positions,
    reset_ball.

% Reset the ball to the center after a goal
reset_ball :-
    retract(ball(_)),
    assertz(ball(position(50, 25))).

% Reset all player positions after a goal
reset_positions :-
    % Team 1
    retract(player(team1, forward1, _, Stamina1, Speed1, Dribbling1, Defending1)),
    assertz(player(team1, forward1, position(30, 20), Stamina1, Speed1, Dribbling1, Defending1)),
    retract(player(team1, forward2, _, Stamina2, Speed2, Dribbling2, Defending2)),
    assertz(player(team1, forward2, position(30, 30), Stamina2, Speed2, Dribbling2, Defending2)),
    retract(player(team1, defender1, _, Stamina3, Speed3, Dribbling3, Defending3)),
    assertz(player(team1, defender1, position(20, 15), Stamina3, Speed3, Dribbling3, Defending3)),
    retract(player(team1, defender2, _, Stamina4, Speed4, Dribbling4, Defending4)),
    assertz(player(team1, defender2, position(20, 35), Stamina4, Speed4, Dribbling4, Defending4)),
    retract(player(team1, goalkeeper, _, Stamina5, Speed5, Dribbling5, Defending5)),
    assertz(player(team1, goalkeeper, position(0, 25), Stamina5, Speed5, Dribbling5, Defending5)),
    
    % Team 2
    retract(player(team2, forward1, _, Stamina6, Speed6, Dribbling6, Defending6)),
    assertz(player(team2, forward1, position(70, 20), Stamina6, Speed6, Dribbling6, Defending6)),
    retract(player(team2, forward2, _, Stamina7, Speed7, Dribbling7, Defending7)),
    assertz(player(team2, forward2, position(70, 30), Stamina7, Speed7, Dribbling7, Defending7)),
    retract(player(team2, defender1, _, Stamina8, Speed8, Dribbling8, Defending8)),
    assertz(player(team2, defender1, position(80, 15), Stamina8, Speed8, Dribbling8, Defending8)),
    retract(player(team2, defender2, _, Stamina9, Speed9, Dribbling9, Defending9)),
    assertz(player(team2, defender2, position(80, 35), Stamina9, Speed9, Dribbling9, Defending9)),
    retract(player(team2, goalkeeper, _, Stamina10, Speed10, Dribbling10, Defending10)),
    assertz(player(team2, goalkeeper, position(100, 25), Stamina10, Speed10, Dribbling10, Defending10)),
    format('All players reset to their starting positions.~n').

% Simulate one round of the game
simulate_round :-
    game_state(round, Round),
    (goal_scored(team1) ; goal_scored(team2)),
    !.

simulate_round :-
    game_state(round, Round),
    (Round =:= 31 -> 
        check_half_time,
        reset_positions,  % Reset player positions at halftime
        retract(game_state(round, 31)),
        assertz(game_state(round, 32)),
        format('~nRound 31 (Second Half Begins):~n')
    ;
        format('~nRound ~w:~n', [Round])),
    
    % Players attempt to kick the ball (only if not resting)
    (kick_ball(team1, forward1) ; kick_ball(team1, forward2) ; 
     kick_ball(team2, forward1) ; kick_ball(team2, forward2) ; true),
    
    % Clear previous round's rest status
    retractall(rested_last_round(_, _)),
    
    % Goalkeeper actions
    (goalkeeper_actions(team1) ; goalkeeper_actions(team2) ; true),
    
    % All players move or rest
    move_and_report(team1, forward1),
    move_and_report(team1, forward2),
    move_and_report(team1, defender1),
    move_and_report(team1, defender2),
    move_and_report(team2, forward1),
    move_and_report(team2, forward2),
    move_and_report(team2, defender1),
    move_and_report(team2, defender2),
    
    % Display ball position
    ball(position(BX, BY)),
    format('Ball is now at (~w, ~w)~n', [BX, BY]),
    
    % Increment round counter if not just after halftime
    (Round =\= 31 ->
        retract(game_state(round, Round)),
        NewRound is Round + 1,
        assertz(game_state(round, NewRound))
    ;
        true).

% Helper predicate to move and report player status
move_and_report(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending),
    % Check if player rested last round
    \+ rested_last_round(Team, Role),
    % Determine rest probability based on stamina
    ( Stamina < 10 -> RestChance is 100
    ; Stamina < 30 -> RestChance is 25
    ; Stamina < 50 -> RestChance is 10
    ; RestChance is 0
    ),
    random(100) < RestChance,
    !,  % Cut to prevent backtracking if resting
    NewStamina is Stamina + 10,
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending)),
    assertz(player(Team, Role, position(X1, Y1), NewStamina, Speed, Dribbling, Defending)),
    assertz(rested_last_round(Team, Role)),
    format('~w ~w rests, stamina restored to ~w~n', [Team, Role, NewStamina]).

move_and_report(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending),
    ball(position(X2, Y2)),
    (Role = defender -> 
        (goal_position(Team, GoalX, _),
         abs(X1 - GoalX) =< 20) -> 
            (abs(X2 - X1) =< 15 -> 
                XDiff is X2 - X1, YDiff is Y2 - Y1
            ;
                XDiff is 0, YDiff is 0)
        ;
        XDiff is X2 - X1, YDiff is Y2 - Y1
    ),
    sign(XDiff, DX), sign(YDiff, DY),
    should_dribble(Team, Role, AdjustedSpeed),  % Adjust speed for dribbling
    NewX is X1 + (DX * AdjustedSpeed),
    NewY is Y1 + (DY * AdjustedSpeed),
    field(size(FieldWidth, FieldHeight)),
    NewX >= 0, NewX =< FieldWidth,
    NewY >= 0, NewY =< FieldHeight,
    BaseStamina is Stamina - AdjustedSpeed,
    (random(20) =:= 0 -> 
        NewStamina is BaseStamina - 10,
        InjuryMessage = ' and gets injured (stamina -10)'
    ; 
        NewStamina is BaseStamina,
        InjuryMessage = ''
    ),
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending)),
    assertz(player(Team, Role, position(NewX, NewY), NewStamina, Speed, Dribbling, Defending)),
    format('~w ~w moves to (~w, ~w), stamina ~w~w~n', [Team, Role, NewX, NewY, NewStamina, InjuryMessage]).

% Check for half-time and reset stamina
check_half_time :-
    format('~nHalf time!~n'),
    % Reset ball to center
    retract(ball(_)),
    assertz(ball(position(50, 25))),
    format('Ball is reset to center (50, 25) for second half~n'),
    % Reset stamina for all players to 90
    (player(Team, Role, Pos, Stamina, Speed, Dribbling, Defending),
     retract(player(Team, Role, Pos, Stamina, Speed, Dribbling, Defending)),
     assertz(player(Team, Role, Pos, 90, Speed, Dribbling, Defending)),
     format('~w ~w stamina reset to 90~n', [Team, Role]),
     fail
    ; true),
    format('Stamina reset complete for all players.~n').

% Run the simulation for all rounds
run_simulation :-
    game_state(round, Round),
    (Round > 60 -> 
        game_state(score(team1, Score1)),
        game_state(score(team2, Score2)),
        format('Game over! Final score: ~w ~w - ~w ~w~n', 
               [team1, Score1, team2, Score2])
    ;
        simulate_round,
        run_simulation).

% Initialize and run the game
start_game :-
    retractall(ball(_)),
    retractall(player(_, _, _, _, _, _, _)),
    retractall(game_state(_, _)),
    retractall(rested_last_round(_, _)),  % Clear any existing rest states
    
    assertz(ball(position(50, 25))),
    
    % Initialize Team 1 players
    assertz(player(team1, forward1, position(30, 20), 100, 3, 70, 30)),
    assertz(player(team1, forward2, position(30, 30), 100, 3, 65, 35)),
    assertz(player(team1, defender1, position(20, 15), 100, 2, 40, 80)),
    assertz(player(team1, defender2, position(20, 35), 100, 2, 45, 75)),
    assertz(player(team1, goalkeeper, position(0, 25), 100, 1, 30, 90)), % Higher dribbling for better kicks
    
    % Initialize Team 2 players
    assertz(player(team2, forward1, position(70, 20), 100, 3, 75, 25)),
    assertz(player(team2, forward2, position(70, 30), 100, 3, 80, 20)),
    assertz(player(team2, defender1, position(80, 15), 100, 2, 35, 85)),
    assertz(player(team2, defender2, position(80, 35), 100, 2, 30, 90)),
    assertz(player(team2, goalkeeper, position(100, 25), 100, 1, 35, 95)), % Higher dribbling for better kicks
    
    % Initialize game state
    assertz(game_state(round, 1)),
    assertz(game_state(score(team1, 0))),
    assertz(game_state(score(team2, 0))),
    
    format('Game started!~n'),
    run_simulation.
