% Define the soccer field dimensions
field(size(100, 50)).

% Define the goal positions
goal_position(team1, 100, 25).  % Team 1's goal at the right end
goal_position(team2, 0, 25).    % Team 2's goal at the left end

% Define the ball's initial position
:- dynamic ball/1.
ball(position(50, 25)).  % Start at the center

% Define player positions and states (stamina and speed)
:- dynamic player/5.
player(team1, forward, position(10, 30), 100, 3).  % Team 1 forward
player(team1, defender, position(20, 20), 100, 2). % Team 1 defender
player(team1, goalkeeper, position(0, 25), 100, 1). % Team 1 goalkeeper

player(team2, forward, position(90, 30), 100, 3).  % Team 2 forward
player(team2, defender, position(80, 20), 100, 2). % Team 2 defender
player(team2, goalkeeper, position(100, 25), 100, 1). % Team 2 goalkeeper

% Move towards the ball only if stamina is sufficient
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed),
    Stamina >= Speed,  % Check if player has enough stamina to move
    ball(position(X2, Y2)),
    XDiff is X2 - X1, YDiff is Y2 - Y1,
    sign(XDiff, DX), sign(YDiff, DY),
    NewX is X1 + (DX * Speed),
    NewY is Y1 + (DY * Speed),
    field(size(FieldWidth, FieldHeight)),
    NewX >= 0, NewX =< FieldWidth,
    NewY >= 0, NewY =< FieldHeight,
    NewStamina is Stamina - Speed,
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed)),
    assertz(player(Team, Role, position(NewX, NewY), NewStamina, Speed)),
    format('~w ~w moves to (~w, ~w), stamina ~w~n', [Team, Role, NewX, NewY, NewStamina]),
    !.  % Cut to prevent backtracking to the next clause

% If stamina is insufficient, player stays put
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X, Y), Stamina, _),
    format('~w ~w is too tired to move (stamina ~w)~n', [Team, Role, Stamina]).

% Kick the ball toward the opponent's goal
kick_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), _, _),
    ball(position(X2, Y2)),
    abs(X1 - X2) =< 10, abs(Y1 - Y2) =< 10,  % Check proximity
    % Get the opponent's goal position
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    XDiff is GoalX - X2, YDiff is GoalY - Y2,
    sign(XDiff, DX), sign(YDiff, DY),
    % Move the ball toward the opponent's goal
    NewBallX is X2 + (DX * 5),  % Move 5 units toward the opponent's goal
    NewBallY is Y2 + (DY * 5),
    % Ensure the ball does not go beyond the field boundaries
    field(size(FieldWidth, FieldHeight)),
    NewBallX >= 0, NewBallX =< FieldWidth,
    NewBallY >= 0, NewBallY =< FieldHeight,
    retract(ball(position(X2, Y2))),
    assertz(ball(position(NewBallX, NewBallY))),
    format('~w ~w kicks the ball to (~w, ~w)~n', [Team, Role, NewBallX, NewBallY]).

% Goalkeeper catches the ball if it's close enough
catch_ball(Team) :-
    player(Team, goalkeeper, position(X, Y), _, _), 
    ball(position(BX, BY)),
    abs(X - BX) =< 2, abs(Y - BY) =< 2,
    retract(ball(position(BX, BY))),
    assertz(ball(position(X, Y))),  % Reset ball to goalkeeper's position
    format('~w goalkeeper catches the ball!~n', [Team]).

% Goal detection
goal_scored(Team) :-
    ball(position(X, Y)),
    goal_position(OtherTeam, GoalX, GoalY),
    Team \= OtherTeam,
    % Check if the ball is very close to the goal line (X-coordinate)
    abs(X - GoalX) =< 2,
    % Check if the ball is within the goal's Y-coordinate range (e.g., between 20 and 30)
    GoalYMin is GoalY - 5,  % Goal height range: 10 units (e.g., 20 to 30)
    GoalYMax is GoalY + 5,
    Y >= GoalYMin, Y =< GoalYMax,
    format('GOAL! ~w scores!~n', [Team]),
    reset_ball.

% Reset the ball to the center after a goal
reset_ball :-
    retract(ball(_)),
    assertz(ball(position(50, 25))),
    format('Ball reset to center.~n').

% Sign function for movement
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

% Simulate one round of the game
simulate_round :-
    (goal_scored(team1) ; goal_scored(team2) ; true),  % Check if a goal is scored
    (kick_ball(team1, forward) ; kick_ball(team2, forward) ; true),  % Try kicking the ball
    move_towards_ball(team1, forward),
    move_towards_ball(team1, defender),
    move_towards_ball(team2, forward),
    move_towards_ball(team2, defender),
    (catch_ball(team1) ; catch_ball(team2) ; true),  % Goalkeeper attempts to catch
    ball(position(BX, BY)),
    format('Ball is now at (~w, ~w)~n', [BX, BY]).

% Run the simulation for N rounds
run_simulation(0).
run_simulation(N) :-
    N > 0,
    simulate_round,
    N1 is N - 1,
    run_simulation(N1).