%% This file contains Erlang code for testing the
%% Erlware emacs mode. It contains functions that
%% use different corner cases to make sure the mode
%% does the right thing.
%% 
%% <<If comment-multi-line is on, Press Return to get another %% line>>

-module(test.erl).

func1() ->
    % This line should stay here after a tab.
    % If the comment-multi-line variable is non-nil, then pressing
    % Return at end of this line shouldto get another % line.
    ok.

func2() ->
    % Pressing tabs on all lines should not change indents.
    {0, $q},
    {1, $(},
    {2, $)},
    {3, $>},
    {4, $<},
    {5, $,},
    {6, ${},
    [$% | ["should not be highlighted as a comment"]],
    ok.

func3() ->
    % If you edit the font lock mode to underline function names,
    % there should not be an underline character after the '->'.
    fun() -> ok end.

func4() ->
    try
        module:somefun(monkey, horse)
    catch
        % both these clauses should maintain their indents after tab presses
        error:something ->
            error;
        error:function_clause ->
            error
    end.

func5(X)
  when is_atom(X) ->
    % 'is_atom' should be highlighted as a guard above

    % All functions below should be highlighted as functions, not
    % as guards or bifs. So each entire function name should be
    % highlighted in the same way.
    f:is_atom(),
    g:registered(),
    h:my_registered(),

    % This should be highlighted as a bif.
    registered(),

    % Should be highlighted as a function.
    deregistered().

-spec func6() -> any().
func6() ->
    % 3 lines below should be highlighted as atoms
    '\'atom',
    'atom\'',
    'at\'om',
    % 3 lines below should be highlighted as literals
    $a,
    $A,
    $1,
    V = x,
    % in line below, V should be highlited as a var
    V(),
    Mo = y,
    Vv = v,
    % in line below, Mo and Vv should be highlited as vars
    Mo:Vv(),
    % in line below, Mo as var, bla as func call
    Mo:bla(),
    ok.

func7() ->
    % should keep indentation on tab
    try func1()
    after
        func2()
    end.

func8() ->
    % anatom should be highlighted as an atom, not a string
    "string$", anatom,
    % this comment should be highlighted as a comment
    % following should be highlighted as a string, should indent on tab
"some $a string".

func9(Term, [${|T]) ->
    % above should be highlited correctly
    % all function body lines should not indent further on tab
    ok.

deregistered() ->
    ok.
