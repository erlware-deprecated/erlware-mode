%% This file contains Erlang code for testing the
%% Erlware emacs mode. It contains functions that
%% use different corner cases to make sure the mode
%% does the right thing.
%% 
%% <<If comment-multi-line is on, Press Return to get another %% line>>

-module(test).
-compile(export_all).

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

func5(Xyz)
  when is_atom(Xyz) ->
    % `is_atom' should be highlighted as a guard above

    % All functions below should be highlighted as functions, not
    % as guards or bifs. So each entire function name should be
    % highlighted in the same way.
    f:is_atom(),
    g:registered(),
    h:my_registered(),
    func6(),
    deregistered(),

    % Xyz should be highlighted as a variabla
    AppSpec = Xyz/2,
    % atom_to_list and element should be highlighted as bifs
    atom_to_list(element(1, AppSpec)),

    % These should be highlighted as bifs.
    erlang:registered(),
    registered(),
    hd(tl(tl(hd([a,b,c])))).

% `-spec' should be highlighted as an attribute, i.e. the same way as
% the `-define'

-define(foo,FOO).
-spec func6() -> any().

func6() ->
    % These should be highlighted as atoms
    'VaV',
    'aVa',
    '\'atom',
    'atom\'',
    'at\'om',
    '#1',
    % 3 lines below should be highlighted as literals
    $a,
    $A,
    $1,
    % in line below, V should be highlited as a var
    V = fun() -> hello end,
    V(),
    Mo = yVy,
    Vv = v,
    % in line below, Mo and Vv should be highlited as vars
    Mo:Vv(),
    % in line below, Mo as var, bla as func call
    Mo:bla(),
    % the ':' should no be highlighted
    ets:insert(bla,{'$1',{'$2','$3'}}).

func7() ->
    % should keep indentation on tab
    try func1()
    after
        func7()
    end.

func8() ->
    % anatom should be highlighted as an atom, not a string

    "string\$", anatom,

    % N.B. A '$' at the end of a string must be escaped, or the
    % highlighting will not work. This is of course a bug, but I don't
    % know how to fix it and the workaround is simple.

    % this comment should be highlighted as a comment
    % following should be highlighted as a string, should indent on tab
    "some $a string".

func9(Term, [${|T]) ->
    % above should be highlighted correctly
    % all function body lines should not indent further on tab
    T.

deregistered() ->
    ok.
