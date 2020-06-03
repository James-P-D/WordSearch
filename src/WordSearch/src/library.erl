% c("library.erl").

-module(library).
-export([search/2]).

%--------------------------------------------------------------------------------------------------------------------------

% Check is parameter1 appears at start of parameter2
match_start_string([], _) -> true;  % If parameter1 is empty list, then we have already checked all characters, so must be true
match_start_string(_, []) -> false; % If parameter2 is empty list, then there aren't enough character in parameter2 to match parameter1, so return false
match_start_string([Head|Tail1], [Head|Tail2]) -> match_start_string(Tail1, Tail2); % If heads of both parameters are equal, then the character matches, so check the tails of both parameters
match_start_string(_, _) -> false.  % In all other cases, return false

%--------------------------------------------------------------------------------------------------------------------------

% Check if parameter2 appears in parameter3, setting parameter1 to the offset if true, or -1 otherwise
sub_string(_, _, []) -> -1;                         % If parameter3 is empty then return -1 since string not found
sub_string(N, Str1, Str2) ->                        % In all other scenarios
    case (match_start_string(Str1, Str2)) of        % Check is parameter2 appears at start of parameter3...
        true -> N;                                  % ..if it does, return true, since we have found the sub-string..
        false -> sub_string(N + 1, Str1, tl(Str2))  % ..if not, increment N to point to next character in parameter3, and recursively search the tail of parameter3
    end.

%--------------------------------------------------------------------------------------------------------------------------

% Gets Nth element
get_nth([], _, _) -> [];
get_nth([Head|Tail], N, M) -> case (N == M) of
                                  true -> Head;
                                  false -> get_nth(Tail, N, M+1)
                              end.
get_nth(L, N) -> get_nth(L, N, 0).

%--------------------------------------------------------------------------------------------------------------------------

% Gets specific row
get_row([], _, _) -> [];
get_row([Head|Tail], N, M) -> case (N == M) of
                                  true -> Head;
                                  false -> get_row(Tail, N, M+1)
                              end.
get_row(L, N) -> get_row(L, N, 0).

%--------------------------------------------------------------------------------------------------------------------------

% Gets specific column
get_col([], _) -> [];
get_col([Head|Tail], N) -> Nth = get_nth(Head, N),
                           if (Nth == []) -> [];
                               true -> [Nth | get_col(Tail, N)]
                           end.

%--------------------------------------------------------------------------------------------------------------------------

% Gets specific diagonal line
get_diag([], _, _) -> [];
get_diag(L, N, M) -> Nth = get_nth(get_row(L, N), M),
                     if (Nth == []) -> [];
                         true -> [Nth | get_diag(tl(L), N, M+1)]
                     end.

%--------------------------------------------------------------------------------------------------------------------------

% Search all rows
search_rows(_, [], _) -> {-1, -1};                                                      % Base case. Nothing more to search so return {-1, -1} to signify word not found in rows
search_rows(L1, [Head|Tail], Row) -> Col = sub_string(0, L1, Head),                     % Check to see if 'L1' appears in the current row (Head will be the first row in the grid)
                                     if (Col == -1) -> search_rows(L1, Tail, Row + 1);  % If it doesn't, then increment the Row counter and search the Tail of the grid
                                         true -> {Row, Col}                             % If it does, then return the Row and Column
                                     end.
search_rows(L1, L2) -> search_rows(L1, L2, 0).                                          % Start searching the grid row-by-row, starting at Row zero

%--------------------------------------------------------------------------------------------------------------------------

% Search all columns
search_cols(_, [], _) -> {-1, -1};                                                      % Base case. Nothing more to search so return {-1, -1} to signify word not found in columns
search_cols(L1, L2, Col) -> WholeColumn = get_col(L2, Col),                             % Get the whole column
                            if (WholeColumn == []) -> {-1, -1};                         % If it's empty, then there's nothing more to search 
                                true -> Row = sub_string(0, L1, WholeColumn),           % Otherwise, search the column
                                        if (Row == -1) -> search_cols(L1, L2, Col + 1); % If word wasn't found, search the next column
                                            true -> {Row, Col}                          % if word was found, return the Row and Column
                                        end
                            end.
search_cols(L1, L2) -> search_cols(L1, L2, 0).                                          % Start searching the grid column-by-column, starting at Column zero

%--------------------------------------------------------------------------------------------------------------------------

% Search all diagonals
search_diags(_, [], _, _) -> {-1, -1};                                                                           % Base case. Nothing more to search so return {-1, -1} to signify word not found in diagonals
search_diags(L1, L2, Row, Col) -> WholeDiag = get_diag(L2, Row, Col),                                            % Get whole diagonal line starting at (Col, Row)
                                  if (WholeDiag == []) -> {-1, -1};                                              % If it's empty, then nothing more to search
                                      true -> RowAndCol = sub_string(0, L1, WholeDiag),                          % Search the diagonal line (note that return value is for both Row and Column since offset
                                              if (RowAndCol =/= -1) -> {Row + RowAndCol, Col + RowAndCol};       % If we found the word, return Row and Column plus the mutual offset
                                                  true -> if (Col > 0) -> search_diags(L1, L2, Row, Col - 1);    % Otherwise, and if we can still move left-wards, search again from the next diagonal on the left
                                                          true -> if (Row == (length(L2) - 1)) -> {-1, -1};      % If we are at the bottom already, then nothing more to search
                                                                      true -> search_diags(L1, L2, Row + 1, Col) % Otherwise, move down and search again
                                                                  end
                                                          end
                                              end
                                  end.
search_diags(L1, L2) -> search_diags(L1, L2, 0, length(hd(L2)) - 1).                                             % Start searching the grid diagonally starting at top-right (0th (top) row, last (right-most) column)

%--------------------------------------------------------------------------------------------------------------------------

sub_search(L1, L2) -> {Row1, Col1} = search_rows(L1, L2),                                                                           % First search the rows
                      if (Row1 =/= -1) and (Col1 =/= -1) -> {Row1, Col1, Row1, Col1 + length(L1)};                                  % If we found the word, return the start and end position
                          true -> {Row2, Col2} = search_cols(L1, L2),                                                               % Then search the columns
                                  if (Row2 =/= -1) and (Col2 =/= -1) -> {Row2, Col2, Row2 + length(L1), Col2};                      % If we found the word, return the start and end position
                                      true -> {Row3, Col3} = search_diags(L1, L2),                                                  % Finally, search the diagonals
                                          if (Row3 =/= -1) and (Col3 =/= -1) -> {Row3, Col3, Row3 + length(L1), Col3 + length(L1)}; % If we found the word, return the start and end position
                                              true -> {-1, -1, -1, -1}                                                              % ...and if nothing was found, return a 4-tuple of -1s
                                          end
                                  end
                      end.

%--------------------------------------------------------------------------------------------------------------------------

search(L1, L2) -> sub_search(L1, L2).      % Main search function. Searches for L1 in L2