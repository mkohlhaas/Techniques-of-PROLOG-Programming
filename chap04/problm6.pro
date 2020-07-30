% PROBLEM 4.6: Read an unordered rainfall file (recursive version 1)
% ------------------------------------------------------------------
% Copy the files '\chap4\rainfall.dat' and '\chap4\rainfal2.dat'
% from the diskette into your Prolog's directory as
%   'c:\prolgdir\rainfall.dat'  and  'c:\prolgdir\rainfal2.dat'.
% (Here, 'c:\prolgdir' is assumed to be the name of your Prolog's
%  directory).
%
% Consult this program and try the following queries:
%    ?- read_rain('CAL'),listing(rain).
%    ?- read_rain('WAS'),listing(rain).
%    ?- read_rain('ORE'),listing(rain).
%    ?- read_rain('TEX'),listing(rain).
% Repeat the above queries with other states.
% ------------------------------------------------------------------
read_rain(State) :-
    see('rainfall.dat'),      % open file for reading
    read_rec(State),          % read records of State
    seen.                     % close file

read_rec(State) :-
    read(rain(S,C,R)),        % read a record
    (S = State,               % if record has given state
       assertz(rain(S,C,R));  % then store it in memory
       true),                 % otherwise, ignore it and
    read_rec(State).          % continue to read records
read_rec(_).              % If end of file then return.

% PROBLEM 4.6: Read an ordered rainfall file (recursive version 2)
% ----------------------------------------------------------------
% Comment out the above program and remove the symbol % at the
% beginning of each line of the following program.
% Reconsult this program and try the above given queries again.
% ----------------------------------------------------------------
% read_rain(State) :-
%     see('rainfal2.dat'),      % open file for reading
%     skip_rec(State,Record),   % skip records to find State
%     read_rec(State,Record),   % store Record, read next rec
%     seen.                     % close file.
%
% skip_rec(S,rain(S,C,R)) :-
%     read(rain(S,C,R)),!;      % If found S then return
%     skip_rec(S,rain(S,C,R)).  % otherwise skip records
%
% read_rec(S,Rec) :-
%     assertz(Rec),             % store Rec in memory
%     read(rain(S,C,R)),        % read next record of state S
%     read_rec(S,rain(S,C,R)).  % store record, read next rec
% read_rec(S,Rec).     % If no more record of state S return
%
%
% PROBLEM 4.6: Read an unordered rainfall file (using repeat)
% ------------------------------------------------------------------
% Comment out the above program and remove the symbol % at the
% beginning of each line of the following program.
% Reconsult this program and try the above given queries again.
% ------------------------------------------------------------------
% read_rain(State) :-
%     see('rainfall.dat'),    % open file for reading
%     read_rec(State),          % read records of State
%     seen.                     % close file
%
% read_rec(State) :-
%     repeat,                   % Repeat
%     read(Rec),                % read a record
%     check_state(State,Rec).   % check record's state
%
% check_state(S,rain(S,C,R)) :- !,  % if record has state S
%     assertz(rain(S,C,R)),fail.    % then store it and fail
% check_state(_,end_of_file).   % Until end of file
%
%
% PROBLEM 4.6: Read an ordered rainfall file (using repeat)
% ------------------------------------------------------------------
% Comment out the above program and remove the symbol % at the
% beginning of each line of the following program.
% Reconsult this program and try the above given queries again.
% ------------------------------------------------------------------
% read_rain(State) :-
%     see('rainfal2.dat'),      % open file for reading
%     skip_rec(State),          % skip records to find State
%     read_rec(State),          % read next records of State
%     seen.                     % close file
%
% skip_rec(S) :-
%     repeat,                   % repeat
%     read(rain(S,C,R)),        % reading until found state S
%     assertz(rain(S,C,R)).     % store record in memory
%
% read_rec(S) :-
%     repeat,                   % repeat
%     read_state(S).            % read record of state S
%
% read_state(S) :-
%     read(rain(S,C,R)),!,        % if record has state S
%     assertz(rain(S,C,R)),fail.  %  then store it & fail
% read_state(S).                  %  else return
