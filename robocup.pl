field_size(100, 50). % Width x Height
goal(team1, position(0, 25)).  % Left goal
goal(team2, position(100, 25)). % Right goal

ball(position(50, 25)). % Start at center
% Players: team, role, position, stamina
player(team1, goalkeeper, position(10, 25), stamina(100)).
player(team1, forward, position(30, 25), stamina(95)).
player(team2, goalkeeper, position(90, 25), stamina(100)).
player(team2, forward, position(70, 25), stamina(95)).
