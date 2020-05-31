-module(translator).
-export([translate/2]).
-export([list_length/1]).

translate(Word, Language) ->
	case Language of
		'Spanish' -> spanish(Word)
	end.
	
spanish(Word) ->
	case Word of
		'friend' -> 'amigo'
	end.
    
list_length(L) -> length(L).    