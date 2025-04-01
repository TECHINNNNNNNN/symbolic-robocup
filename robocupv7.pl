% Define the soccer field dimensions
field(size(100, 50)).

% Define the goal positions
goal_position(team1, 100, 25).  % Team 1's goal at the right end
goal_position(team2, 0, 25).    % Team 2's goal at the left end

% Declare all dynamic predicates
:- dynamic ball/1.
:- dynamic player/7.
:- dynamic game_state/2.
:- dynamic rested_last_round/2.

% Sign function for movement
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

% Calculate Euclidean distance between two points (in integer)
distance(X1, Y1, X2, Y2, Distance) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    Distance is round(sqrt(DX * DX + DY * DY)).

% Check if a player is close enough to shoot
can_shoot(Team, X, Y) :-
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    distance(X, Y, GoalX, GoalY, Distance),
    Distance =< 30,
    \+ (
        opponent_player(OppX, OppY, _, _),
        between(X, GoalX, OppX),
        abs(OppY - Y) =< 4
    ).

% Check if there are no opponents between player and goal
can_dribble(Team, X, Y) :-
    (Team = team1 -> goal_position(team2, GoalX, _) ; goal_position(team1, GoalX, _)),
    distance(X, GoalX, Y, 25, Distance),  % Distance to goal center
    Distance > 35,
    \+ (player(OtherTeam, _, position(OppX, OppY), _, _, _, _),
        Team \= OtherTeam,
        ((Team = team1, OppX > X, OppX < GoalX) ; (Team = team2, OppX < X, OppX > GoalX)),
        abs(OppY - Y) =< 10).

% Check if a player can pass to a teammate
can_pass(Team, X, Y, CurrentRole, TargetRole) :-
    player(Team, TargetRole, position(TX, TY), _, _, _, _),
    TargetRole \= goalkeeper,
    TargetRole \= CurrentRole,  % Prevent self-passing
    distance(X, Y, TX, TY, PassDistance),
    PassDistance =< 15,
    (Team = team1 -> goal_position(team2, GoalX, _) ; goal_position(team1, GoalX, _)),
    distance(X, Y, GoalX, Y, GoalDistance),
    PassDistance < GoalDistance.

% Move towards the ball
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), Stamina, Speed, Attacking, Defending),
    ball(position(X2, Y2)),
    (Role = defender -> 
        (goal_position(Team, GoalX, _),
         abs(X1 - GoalX) =< 25) ->
            (abs(X2 - X1) =< 20 ->
                XDiff is X2 - X1, YDiff is Y2 - Y1
            ;
                XDiff is 0, YDiff is 0)
        ;
        XDiff is X2 - X1, YDiff is Y2 - Y1
    ),
    sign(XDiff, DX), sign(YDiff, DY),
    NewX is X1 + (DX * Speed),
    NewY is Y1 + (DY * Speed),
    field(size(FieldWidth, FieldHeight)),
    NewX >= 0, NewX =< FieldWidth,
    NewY >= 0, NewY =< FieldHeight,
    NewStamina is Stamina - Speed,
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed, Attacking, Defending)),
    assertz(player(Team, Role, position(NewX, NewY), NewStamina, Speed, Attacking, Defending)).

% Helper predicate to update ball position with bounds checking
update_ball_position(NewX, NewY) :-
    field(size(FieldWidth, FieldHeight)),
    BoundedX is max(0, min(FieldWidth, round(NewX))),
    BoundedY is max(0, min(FieldHeight, round(NewY))),
    retract(ball(position(_, _))),
    assertz(ball(position(BoundedX, BoundedY))).

% Shoot the ball with power
shoot_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), _, _, Attacking, _),
    ball(position(X2, Y2)),
    (Team = team1 -> goal_position(team2, GoalX, GoalY) ; goal_position(team1, GoalX, GoalY)),
    distance(X1, Y1, GoalX, GoalY, Distance),
    BasePower is 40 + (Attacking // 2),
    DistanceFactor is max(6, 9 - (Distance // 10)),
    KickPower is (BasePower * DistanceFactor) // 10,
    
    % Record the shot power for goalkeeper calculations
    retractall(game_state(last_shot_power, _)),
    assertz(game_state(last_shot_power, KickPower)),
    
    DX is GoalX - X1,
    DY is GoalY - Y1,
    RandomX is random(3) - 1,
    RandomY is random(3) - 1,
    NewBallX is X1 + (DX * KickPower // 10) + RandomX,
    NewBallY is Y1 + (DY * KickPower // 10) + RandomY,
    update_ball_position(NewBallX, NewBallY),
    format('~w ~w shoots with power ~w!~n', [Team, Role, KickPower]).

% Pass the ball
pass_ball(Team, Role, TargetRole) :-
    player(Team, Role, _, _, _, Attacking, _),
    player(Team, TargetRole, position(TX, TY), _, _, _, _),
    ball(position(X2, Y2)),
    XDiff is TX - X2, YDiff is TY - Y2,
    sign(XDiff, DX), sign(YDiff, DY),
    PassPower is 3 + (Attacking // 30),
    NewBallX is X2 + (DX * PassPower),
    NewBallY is Y2 + (DY * PassPower),
    update_ball_position(NewBallX, NewBallY),
    format('~w ~w passes to ~w~n', [Team, Role, TargetRole]).

% Dribble the ball
dribble_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), _, _, Attacking, _),
    ball(position(X2, Y2)),
    % Move forward if not too close to goal
    (Team = team1 -> 
        (X1 < 90 -> DX is 1 ; DX is 0)
    ;
        (X1 > 10 -> DX is -1 ; DX is 0)),  
    DribblePower is 1 + (Attacking // 30),
    NewBallX is X2 + (DX * DribblePower),
    NewBallY is Y2 + (random(3) - 1),
    update_ball_position(NewBallX, NewBallY),
    format('~w ~w is dribbling forward~n', [Team, Role]).

% Check if a player is the closest to the ball
is_closest_to_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), _, _, _, _),
    ball(position(X2, Y2)),
    distance(X1, Y1, X2, Y2, MyDistance),
    \+ (player(OtherTeam, OtherRole, position(OtherX, OtherY), _, _, _, _),
        (OtherTeam \= Team ; OtherRole \= Role),
        distance(OtherX, OtherY, X2, Y2, OtherDistance),
        OtherDistance < MyDistance).

% Kick the ball toward the opponent's goal (for outfield players)
kick_ball(Team, Role) :-
    Role \= goalkeeper,
    \+ rested_last_round(Team, Role),
    player(Team, Role, position(X1, Y1), _, _, _, _),
    ball(position(X2, Y2)),
    abs(X1 - X2) =< 5, abs(Y1 - Y2) =< 5,
    (
        % Check if player is in opponent's half and has a clear path to goal
        (Team = team1, X1 > 50 ; Team = team2, X1 < 50),
        can_dribble(Team, X1, Y1) ->
            dribble_ball(Team, Role)
    ;
        % Check if player is in shooting range
        can_shoot(Team, X1, Y1) ->
            shoot_ball(Team, Role)
    ;
        % Check if player can pass to a teammate
        can_pass(Team, X1, Y1, Role, TargetRole) ->
            pass_ball(Team, Role, TargetRole)
    ;
        % If player can dribble, do so
        can_dribble(Team, X1, Y1) ->
            dribble_ball(Team, Role)
    ;
        % Default to dribbling if no other option
        dribble_ball(Team, Role)
    ),
    !,  % Cut to prevent backtracking after an action
    assertz(rested_last_round(Team, Role)).  % Mark this player as having acted this round

% Goalkeeper kick
goalkeeper_kick(Team) :-
    player(Team, goalkeeper, position(GKX, GKY), _, _, _, _),
    ball(position(BX, BY)),
    abs(GKX - BX) =< 5, 
    abs(GKY - BY) =< 5,
    % Goalkeepers kick toward the center line or to a teammate
    (random(3) =:= 0 ->  % 1/3 chance of kicking to center line
        NewBallX is 50,
        NewBallY is 25,
        format('~w goalkeeper makes a long goal kick to center!~n', [Team])
    ;
        % Otherwise kick to a teammate (simplified - just kick forward)
        (Team = team2 -> 
            TargetX is GKX + 40,
            format('~w goalkeeper distributes the ball forward~n', [Team])
        ;
            TargetX is GKX - 40,
            format('~w goalkeeper distributes the ball forward~n', [Team])),
        NewBallY is max(5, min(45, GKY + random(21) - 10)), % Keep Y coordinate within reasonable range
        NewBallX is TargetX
    ),
    update_ball_position(NewBallX, NewBallY).

% Helper predicate to check for opponent players
opponent_player(X, Y, Team, Role) :-
    player(OtherTeam, Role, position(X, Y), _, _, _, _),
    Team \= OtherTeam.

% Goalkeeper actions
goalkeeper_actions(Team) :-
    player(Team, goalkeeper, position(GKX, GKY), _, _, _, Defending),
    ball(position(BX, BY)),
    % Get the last shot power
    (game_state(last_shot_power, Power) -> true ; Power = 30), % Default if not set
    
    % Calculate save probability based on shot power and defending statistics
    % Base save chance is 75%, higher shot power reduces this chance
    BaseSaveChance is 75 + (Defending // 2),
    PowerFactor is max(10, 50 - (Power // 2)),
    SaveChance is (BaseSaveChance * PowerFactor) // 50, % Scale to 0-100 range
    
    % Determine if save is successful
    random(100) < SaveChance,
    !,
    format('~w goalkeeper makes a fantastic save!~n', 
           [Team]),
    goalkeeper_kick(Team).

goalkeeper_actions(Team) :-
    % Save fails
    format('~w goalkeeper fails to react!~n', [Team]),
    fail.

% Goal detection
goal_scored(Team) :-
    % Use once/1 to ensure this predicate can only succeed once for the same ball position
    once((
        ball(position(X, Y)),
        ((Team = team1, X >= 93) ; (Team = team2, X =< 7)),
        (Team = team1 -> goal_position(team2, _, GoalY) ; goal_position(team1, _, GoalY)),
        GoalYMin is GoalY - 12,
        GoalYMax is GoalY + 12,
        Y >= GoalYMin,
        Y =< GoalYMax,
        format('GOAL! ~w scores!~n', [Team]),
        update_score(Team),
        reset_ball
    )).

% Update score helper predicate
update_score(Team) :-
    % Get current scores
    findall(S1, game_state(score(team1, S1)), Team1Scores),
    findall(S2, game_state(score(team2, S2)), Team2Scores),
    (Team1Scores = [] -> OldScore1 = 0 ; last(Team1Scores, OldScore1)),
    (Team2Scores = [] -> OldScore2 = 0 ; last(Team2Scores, OldScore2)),
    
    % Update appropriate team's score
    (Team = team1 ->
        NewScore1 is OldScore1 + 1,
        NewScore2 = OldScore2,
        retractall(game_state(score(team1, _))),
        assertz(game_state(score(team1, NewScore1)))
    ;
        NewScore1 = OldScore1,
        NewScore2 is OldScore2 + 1,
        retractall(game_state(score(team2, _))),
        assertz(game_state(score(team2, NewScore2)))
    ),
    
    format('Current score: team1 ~w - team2 ~w~n', [NewScore1, NewScore2]),
    !.  % Cut to prevent backtracking after score update

% Reset the ball to the center after a goal
reset_ball :-
    retract(ball(_)),
    assertz(ball(position(50, 25))).

% Reset all player positions after a goal
reset_positions :-
    % Team 1
    retract(player(team1, forward1, _, Stamina1, Speed1, Attacking1, Defending1)),
    assertz(player(team1, forward1, position(70, 20), Stamina1, Speed1, Attacking1, Defending1)),
    retract(player(team1, forward2, _, Stamina2, Speed2, Attacking2, Defending2)),
    assertz(player(team1, forward2, position(70, 30), Stamina2, Speed2, Attacking2, Defending2)),
    retract(player(team1, defender1, _, Stamina3, Speed3, Attacking3, Defending3)),
    assertz(player(team1, defender1, position(80, 15), Stamina3, Speed3, Attacking3, Defending3)),
    retract(player(team1, defender2, _, Stamina4, Speed4, Attacking4, Defending4)),
    assertz(player(team1, defender2, position(80, 35), Stamina4, Speed4, Attacking4, Defending4)),
    retract(player(team1, goalkeeper, _, Stamina5, Speed5, Attacking5, Defending5)),
    assertz(player(team1, goalkeeper, position(100, 25), Stamina5, Speed5, Attacking5, Defending5)),
    
    % Team 2
    retract(player(team2, forward1, _, Stamina6, Speed6, Attacking6, Defending6)),
    assertz(player(team2, forward1, position(30, 20), Stamina6, Speed6, Attacking6, Defending6)),
    retract(player(team2, forward2, _, Stamina7, Speed7, Attacking7, Defending7)),
    assertz(player(team2, forward2, position(30, 30), Stamina7, Speed7, Attacking7, Defending7)),
    retract(player(team2, defender1, _, Stamina8, Speed8, Attacking8, Defending8)),
    assertz(player(team2, defender1, position(20, 15), Stamina8, Speed8, Attacking8, Defending8)),
    retract(player(team2, defender2, _, Stamina9, Speed9, Attacking9, Defending9)),
    assertz(player(team2, defender2, position(20, 35), Stamina9, Speed9, Attacking9, Defending9)),
    retract(player(team2, goalkeeper, _, Stamina10, Speed10, Attacking10, Defending10)),
    assertz(player(team2, goalkeeper, position(0, 25), Stamina10, Speed10, Attacking10, Defending10)),

    game_state(score(team1, S1)),
    game_state(score(team2, S2)),

    format('All players reset to their starting positions.~n').

% Simulate one round of the game
simulate_round :-
    game_state(round, Round),
    (Round =:= 31 -> % First check for halftime
        check_half_time,
        reset_positions,
        retract(game_state(round, 31)),
        assertz(game_state(round, 32)),
        format('~nRound 31 (Second Half Begins):~n')
    ;
        format('~nRound ~w:~n', [Round])),
    
    % Display current score at the start of each round
    show_current_score,
    
    % Process ball actions first (only one per round)
    process_ball_actions,
    
    % Process player movements (exactly once per player)
    process_all_player_movements,
    
    % Display ball position
    ball(position(BX, BY)),
    format('Ball is now at (~w, ~w)~n', [BX, BY]),
    
    % Increment round counter
    (Round =\= 31 ->
        retract(game_state(round, Round)),
        NewRound is Round + 1,
        assertz(game_state(round, NewRound))
    ;
        true).

% Helper predicate to show current score
show_current_score :-
    findall(S1, game_state(score(team1, S1)), Team1Scores),
    findall(S2, game_state(score(team2, S2)), Team2Scores),
    (Team1Scores = [] -> Score1 = 0 ; last(Team1Scores, Score1)),
    (Team2Scores = [] -> Score2 = 0 ; last(Team2Scores, Score2)),
    format('Current score: team1 ~w - team2 ~w~n', [Score1, Score2]).

% Process ball actions (goalkeeper action, goal check, closest player action)
process_ball_actions :-
    % First check for goalkeeper actions
    (ball(position(BX, BY)),
     ((BX >= 95, goalkeeper_actions(team1)) ; 
      (BX =< 5, goalkeeper_actions(team2)) ; true)),
    
    % Then check for goals
    once((goal_scored(team1) ; goal_scored(team2) ; true)),
    
    % Finally find the closest player to the ball who can act
    (find_closest_player(Team, Role),
     kick_ball(Team, Role) ; true),
    !.

% Process all player movements in sequence
process_all_player_movements :-
    % Clear previous round's rest status
    retractall(rested_last_round(_, _)),
    
    % Process each player's movement exactly once
    process_player_movement(team1, forward1),
    process_player_movement(team1, forward2),
    process_player_movement(team1, defender1),
    process_player_movement(team1, defender2),
    process_player_movement(team2, forward1),
    process_player_movement(team2, forward2),
    process_player_movement(team2, defender1),
    process_player_movement(team2, defender2).

% Process a single player's movement for the round
process_player_movement(Team, Role) :-
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

process_player_movement(Team, Role) :-
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
    NewX is X1 + (DX * Speed),
    NewY is Y1 + (DY * Speed),
    field(size(FieldWidth, FieldHeight)),
    NewX >= 0, NewX =< FieldWidth,
    NewY >= 0, NewY =< FieldHeight,
    BaseStamina is Stamina - Speed,
    (random(50) =:= 0 ->  % Injury chance of 1/50
        NewStamina is BaseStamina - 10,
        InjuryMessage = ' and gets injured (stamina -10)'
    ; 
        NewStamina is BaseStamina,
        InjuryMessage = ''
    ),
    retract(player(Team, Role, position(X1, Y1), Stamina, Speed, Dribbling, Defending)),
    assertz(player(Team, Role, position(NewX, NewY), NewStamina, Speed, Dribbling, Defending)),
    format('~w ~w moves to (~w, ~w), stamina ~w~w~n', [Team, Role, NewX, NewY, NewStamina, InjuryMessage]),
    !.  % Cut to ensure only one movement per player per round

% Helper predicate to find the closest player to the ball
find_closest_player(Team, Role) :-
    ball(position(BX, BY)),
    player(Team, Role, position(X, Y), _, _, _, _),
    distance(X, Y, BX, BY, MyDistance),
    \+ (player(OtherTeam, OtherRole, position(OtherX, OtherY), _, _, _, _),
        (OtherTeam \= Team ; OtherRole \= Role),
        distance(OtherX, OtherY, BX, BY, OtherDistance),
        OtherDistance < MyDistance).

% Check for half-time
check_half_time :-
    format('~nHalf time!~n'),
    % Get current scores
    findall(S1, game_state(score(team1, S1)), Team1Scores),
    findall(S2, game_state(score(team2, S2)), Team2Scores),
    (Team1Scores = [] -> Score1 = 0 ; last(Team1Scores, Score1)),
    (Team2Scores = [] -> Score2 = 0 ; last(Team2Scores, Score2)),
    format('Half-time score: team1 ~w - team2 ~w~n', [Score1, Score2]),

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
        % Get the MOST RECENT scores for each team
        findall(S1, game_state(score(team1, S1)), Team1Scores),
        findall(S2, game_state(score(team2, S2)), Team2Scores),
        last(Team1Scores, FinalScore1),  % Take the last recorded score
        last(Team2Scores, FinalScore2),
        format('Game over! Final score: ~w ~w - ~w ~w~n', 
               [team1, FinalScore1, team2, FinalScore2])
    ;
        simulate_round,
        sleep(0.5),  % 0.5 seconds pause before next round
        run_simulation).

% Initialize and run the game
start_game :-
    retractall(ball(_)),
    retractall(player(_, _, _, _, _, _, _)),
    retractall(game_state(_, _)),
    retractall(rested_last_round(_, _)),
    
    assertz(ball(position(50, 25))),
    
    % Initialize Team 1 players
    assertz(player(team1, forward1, position(70, 20), 100, 3, 80, 35)),
    assertz(player(team1, forward2, position(70, 30), 100, 3, 75, 40)),
    assertz(player(team1, defender1, position(80, 15), 100, 2, 45, 85)),
    assertz(player(team1, defender2, position(80, 35), 100, 2, 50, 80)),
    assertz(player(team1, goalkeeper, position(100, 25), 100, 1, 30, 90)),
    
    % Initialize Team 2 players with balanced stats
    assertz(player(team2, forward1, position(30, 20), 100, 3, 80, 35)),
    assertz(player(team2, forward2, position(30, 30), 100, 3, 75, 40)),
    assertz(player(team2, defender1, position(20, 15), 100, 2, 45, 85)),
    assertz(player(team2, defender2, position(20, 35), 100, 2, 50, 80)),
    assertz(player(team2, goalkeeper, position(0, 25), 100, 1, 30, 90)),
    
    % Initialize game state
    assertz(game_state(round, 1)),
    assertz(game_state(score(team1, 0))),
    assertz(game_state(score(team2, 0))),
    
    format('Game started!~n'),
    run_simulation.