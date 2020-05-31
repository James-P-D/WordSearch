% c("library.erl").
% library:stuff([N, "abc", "abcdef"]).      % Returns true, N = 0
% library:stuff([N, "abc", "xyzabcdef"]).   % Returns true, N = 3
% library:stuff([N, "abc", "xyzabcdef"]).   % Returns false, N = -1

-module(library).
-export([stuff/2]).

% Check is parameter1 appears at start of parameter2
match_start_string([], _) -> true;  % If parameter1 is empty list, then we have already checked all characters, so must be true
match_start_string(_, []) -> false; % If parameter2 is empty list, then there aren't enough character in parameter2 to match parameter1, so return false
match_start_string([Head|Tail1], [Head|Tail2]) -> match_start_string(Tail1, Tail2); % If heads of both parameters are equal, then the character matches, so check the tails of both parameters
match_start_string(_, _) -> false.  % In all other cases, return false

% Check if parameter2 appears in parameter3, setting parameter1 to the offset if true, or -1 otherwise
sub_string(_, _, []) -> -1;                         % If parameter3 is empty then return -1 since string not found
sub_string(N, Str1, Str2) ->                        % In all other scenarios
    case (match_start_string(Str1, Str2)) of        % Check is parameter2 appears at start of parameter3...
        true -> N;                                  % ..if it does, return true, since we have found the sub-string..
        false -> sub_string(N + 1, Str1, tl(Str2))  % ..if not, increment N to point to next character in parameter3, and recursively search the tail of parameter3
    end.

display([]) -> [];
display([Head|Tail]) -> io:fwrite("~c ", [Head]), display(Tail).
display_nl(X) -> display(X), io:fwrite("~n").

stuff(Word, Sentence) ->    
    N = sub_string(0, Word, Sentence),
    case N of
        -1 -> io:fwrite("Not a match!~n");
        _ -> io:fwrite("N = ~B~n", [N]), io:fwrite("Match!~n")
    end.