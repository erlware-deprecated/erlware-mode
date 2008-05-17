%% This file contains Erlang code for testing the
%% Erlware emacs mode. It contains functions that
%% use different corner cases to make sure the mode
%% does the right thing.
%% 
%% <<Press Return at end of line to get another %% line>>

-module(test.erl).

func1() ->
    % This line should stay here after a tab.
    % <<Press Return at end of line to get another % line>>
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
    % This is currently broken. If you edit the font lock
    % mode to underline function names, there will be an
    % underline character after the 'ok'.
    fun() -> ok end.
