%% This file contains Erlang code for testing the
%% Erlware emacs mode. It contains functions that
%% use different corner cases to make sure the mode
%% does the right thing.
%% 
%% <<Press Return at end of line to get another %% line>>

-module(test.erl).

func1() ->
    % This line should stay here after a tab.
    % If the comment-multi-line variable is non-nil, then pressing
    % Return at end of this line shouldto get another % line.
    ok.

func2() ->
    % Pressing tabs on all lines should not change indents.
    {1, $(},
    {2, $)},
    {3, $>},
    {4, $<},
    {5, $,},
    [$% | ["should not be highlighted as a comment"]],
    ok.

func3() ->
    % If you edit the font lock mode to underline function names,
    % there should not be an underline character after the '->'.
    fun() -> ok end.

func4() ->
    try
        module:function(monkey, horse)
    catch
        % both these clauses should maintain their indents after tab presses
        error:something ->
            error;
        error:function_clause ->
            error
    end.
