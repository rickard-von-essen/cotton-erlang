-module(integration_test).

-include_lib("eunit/include/eunit.hrl").

-define(Url, "http://localhost:2345/").
-define(Mime, "x-application/hessian").

-record(pair, {fqn= <<"net.sf.cotton.records.Pair">>, first, second }).

add_test() ->
    State = [],
    Call = hessian:encode(call, <<"add">>, [2,3] , State),
    Result = hessian:decode( send_call(Call) , State),
    ?assertMatch( 5, Result ).


% These tests will only work when the Hessian 2.0 object encoding is in place

%% record_in_test() ->
%%     State = registry(),
%%     Pair = #pair{first = <<"foo">>, second = <<"bar">>},
%%     Call = hessian:encode(call, <<"concatenate">>, [Pair] , State ),
%%     Result = hessian:decode( send_call(Call), State ),
%%     ?assertMatch( <<"foobar">>, Result ).


%% record_out_test() ->
%%     Q = <<114,2,0,79,170,110,101,116,46,115,102,46,99,111,116,116,111,110,46,114,101,
%%   99,111,114,100,115,46,80,97,105,114,146,5,102,105,114,115,116,6,115,101,99,
%%   111,110,100,111,144,3,102,111,111,3,98,97,114,122>>,
%%     hessian:decode(Q).
%%     Call = hessian:encode(call, <<"split">>, [<<"foobar">>]),
%%     R = send_call(Call),
%%     io:format("~p~n",[R]),
%%     Dict = hessian:decode( R ),
%%     ?assertMatch( <<"net.sf.cotton.records.Pair">> , dict:fetch(fqn, Dict) ),
%%     ?assertMatch( <<"foo">> , dict:fetch( <<"first">>, Dict) ),
%%     ?assertMatch( <<"bar">> , dict:fetch( <<"second">>, Dict) ).

%%
%% Utility methods
%%
send_call(Call) ->
    inets:start(),
    {ok, {{Version, ReplyCode, ReasonPhrase}, Headers, Response}} =
      http:request(post, {?Url, [], ?Mime , Call}, [], []),
    list_to_binary(Response).

registry() ->
    FieldNames = record_info(fields, pair),
    Dict0 = dict:new(),
    dict:store(pair, FieldNames, Dict0).
