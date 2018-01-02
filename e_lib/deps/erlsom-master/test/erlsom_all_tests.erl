-module(erlsom_all_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

compile_xsd() ->
    erlsom:compile_xsd_file(file_path("all.xsd"), []).

parse_file(FileName, Model) ->
    erlsom:parse_file(file_path(FileName), Model).

file_path(File) ->
    filename:join([code:priv_dir(erlsom), all, File]).


stability_test_() ->
    [ stability_case("all.xml") ].


stability_case(FileName) ->
    {ok, Model} = compile_xsd(),
    {ok, Tree1} = parse_file(FileName, Model),
    {ok, XML}   = erlsom:write(Tree1, Model),
    {ok, Tree2} = erlsom:parse(XML, Model),
    {FileName, ?_assertEqual(Tree1, Tree2)}.
