% c("library.erl").
% library:stuff(["abc", "abcdef"]).
% library:stuff(["abc", "xyzabcdef"]).

-module(library).
-export([stuff/2]).

match_start_string([], _) -> true;
match_start_string(_, []) -> false;
match_start_string([Head|Tail1], [Head|Tail2]) -> match_start_string(Tail1, Tail2);
match_start_string(_, _) -> false.

get_tail([_|Tail]) -> Tail.

sub_string(_, _, []) -> -1;
sub_string(N, Str1, Str2) ->
    case (match_start_string(Str1, Str2)) of
        true -> N;
        false -> sub_string(N + 1, Str1, get_tail(Str2))
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