-module(hessian_test).

-include("hessian.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([test_coverage/0]).
-export([roundtrip/3]).

-record(test_record, {fqn = <<"net.sf.cotton.TestRecord">>, first, second, third }).
-record(linked_list, {fqn = <<"LinkedList">>, head, tail }).
-record(a, {name, b}).
-record(b, {number}).

%---------------------------------------------------------------------------
% Hessian Serialization Grammar: Individual type encoding / decoding
%---------------------------------------------------------------------------

binary_test() ->
    roundtrip(binary, <<>>, <<16#20>>),
    roundtrip(binary, <<1,2,3>>, <<16#23,1,2,3>>),
    crypto:start(),
    NonFinalChunk1 = crypto:rand_bytes(?CHUNK_SIZE),
    NonFinalChunk2 = crypto:rand_bytes(?CHUNK_SIZE),
    FinalChunk = crypto:rand_bytes(?CHUNK_SIZE div 2),
    crypto:stop(),
    NonFinalChunkBin1 = <<$b,?CHUNK_SIZE:16,NonFinalChunk1/binary>>,
    NonFinalChunkBin2 = <<$b,?CHUNK_SIZE:16,NonFinalChunk2/binary>>,
    FinalChunkBin = <<$B,(?CHUNK_SIZE div 2):16,FinalChunk/binary>>,
    Encoded = <<NonFinalChunkBin1/binary,NonFinalChunkBin2/binary,FinalChunkBin/binary>>,
    Native = list_to_binary([NonFinalChunk1,NonFinalChunk2, FinalChunk]),
    roundtrip(binary, Native, Encoded).

boolean_test() ->
    roundtrip(boolean, true, <<$T>>),
    roundtrip(boolean, false, <<$F>>).

date_test() ->
    State = [],
    Date = {{1998,5,8},{2,31,51}},
    Bin = <<100,0,0,0,208,73,201,15,88>>,
    Bytes = hessian:encode(localtime, Date,State),
    ?assertMatch(Bin,Bytes),
    {Rest, _Date, State} = hessian:decode(Bin,State),
    UTime = calendar:now_to_local_time(_Date),
    ?assertMatch( Date, UTime ).

double_test() ->
    State = [],
    roundtrip(double, 0.0, <<16#67>>),
    roundtrip(double, 1.0, <<16#68>>),
    ?assertMatch( {<<>>, 0.0, State}, hessian:decode(<<16#69,0>>, State)),
    roundtrip(double, 127.0, <<16#69,16#7f>>),
    roundtrip(double, -128.0, <<16#69,16#80>>),
    roundtrip(double, -1.0, <<16#69,16#ff>>),
    ?assertMatch( {<<>>, 0.0, State}, hessian:decode(<<16#6a,0,0>>, State)),
    roundtrip(double, -32768.0, <<16#6a,16#80,0>>),
    roundtrip(double, 32767.0, <<16#6a,16#7f,16#ff>>),
    roundtrip(double, 12.25, <<16#6b,64,40,128,0>>),
    roundtrip(double, 214.3, <<$D,64,106,201,153,153,153,153,154>>).

int_test() ->
    State = [],
    roundtrip(int, 0, <<16#90>>),
    roundtrip(int, -16, <<16#80>>),
    roundtrip(int, 47, <<16#bf>>),
    roundtrip(int, 2047, <<16#cf,16#ff>>),
    roundtrip(int, 262143, <<16#d7,16#ff,16#ff>>),
    roundtrip(int, 262144257, <<$I,15,160,1,1>>),
    ?assertMatch( {<<>>, 0, State}, hessian:decode(<<16#c8,0>>, State)),
    ?assertMatch( {<<>>, 0, State}, hessian:decode(<<16#d4,0,0>>, State)),
    ?assertMatch( {<<>>, 0, State}, hessian:decode(<<$I,0,0,0,0>>, State)),
    ?assertMatch( {<<>>, 300, State}, hessian:decode(<<$I,0,0,1,16#2c>>, State)).

long_test() ->
    State = [],
    roundtrip(long, 0, <<16#e0>>),
    roundtrip(long, -8, <<16#d8>>),
    roundtrip(long, 15, <<16#ef>>),
    roundtrip(long, -2048, <<16#f0,0>>),
    roundtrip(long, -256, <<16#f7,0>>),
    roundtrip(long, 2047, <<16#ff,16#ff>>),
    roundtrip(long, -262144, <<16#38,0,0>>),
    roundtrip(long, 262143, <<16#3f,16#ff,16#ff>>),
    roundtrip(long, 16#ffffffff, <<16#77,16#ff,16#ff,16#ff,16#ff>>),
    roundtrip(long, 661474839648, <<$L,0,0,0,154,2,249,12,96>>),
    ?assertMatch( {<<>>, 0, State}, hessian:decode(<<16#f8,0>>, State)),
    ?assertMatch( {<<>>, 0, State}, hessian:decode(<<16#3c,0,0>>, State)),
    ?assertMatch( {<<>>, 0, State}, hessian:decode(<<16#77,0,0,0,0>>, State)),
    ?assertMatch( {<<>>, 300, State}, hessian:decode(<<16#77,0,0,16#1,16#2c>>, State)).

null_test() ->
    State = [],
    Encoded = hessian:encode(undefined, State),
    ?assertMatch(<<$N>>, Encoded),
    {<<>>, Decoded, State} = hessian:decode(<<$N>>, State),
    ?assertMatch(undefined, Decoded).

string_test() ->
    State = [],
    roundtrip(string, <<>>, <<0>>),
    roundtrip(string, <<"hello">>, <<16#05,"hello">>),
    roundtrip(string, <<16#c3>>, <<16#02,16#c3,16#83>>),
    Native = <<"Seacula quarta decima et quinta decima Eodem modo typi? Modo typi qui nunc nobis
 videntur parum clari. Quam littera gothica quam nunc putamus parum claram anteposuerit.
 Lorem ipsum dolor sit amet, ligula suspendisse nulla pretium, rhoncus tempor placerat fermentum,
 enim integer ad vestibulum volutpat. Nisl rhoncus turpis est, vel elit, congue wisi enim nunc
 ultricies sit, magna tincidunt. Duis montes, tellus lobortis lacus amet arcu et. Curabitur
 auctor, erat mollis sed fusce, turpis vivamus a dictumst congue magnis. Aliquam amet ullamcorper
 dignissim molestie, gravida mollis. Tortor vitae tortor eros wisi facilisis.Ut enim ad minim veniam,
 quis nostrud exerc. Irure dolor in reprehend incididunt ut labore et dolore magna aliqua.
 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
 consequat. Maecenas aliquam maecenas ligula nostra, accumsan taciti. Sociis mauris in integer, a
 dolor netus non dui aliquet, sagittis felis sodales, dolor sociis mauris, vel eu libero cras.
 Interdum at. In vitae vel, wisi at, id praesent bibendum libero faucibus porta egestas, quisque
 praesent ipsum.Neque pecun modut est neque nonor et imper ned libidig met, consectetur adipiscing
 elit, sed ut labore et dolore magna aliquam is nostrud exercitation ullam mmodo consequet. Eget
 habitasse elementum est, ipsum purus pede porttitor class, ut adipiscing, aliquet sed auctor,
 imperdiet arcu per diam dapibus libero duis. Enim eros in vel, volutpat nec pellentesque leo,
 temporibus scelerisque nec fermentum tempor.Braid, yop quiel geg ba solaly rasponsubla rof
 trenzur sala ent dusgrubuguon. Offoctivo immoriatoly, hawrgasi pwicos asi sirucor. Thas
 sirutciun applios tyu thuso itoms. Eget habitasse elementum est, ipsum purus pede porttitor class,
 ut adipiscing, aliquet sed auctor, imperdiet arcu per diam dapibus libero duis.Nodo typi qui nunc.
 Claram anteposuerit litterarum formas humanitatis per seacula quarta decima et quinta. Sequitur
 mutationem consuetudium lectorum. Enim eros in vel, volutpat nec pellentesque leo, temporibus
 scelerisque nec.Quarta seacula per humanitatis formas litterarum anteposuerit claram parum
 putamus! Litterarum anteposuerit claram parum putamus nunc quam, euismod nibh nonummy diam
 sed elit adipiscing consectetuer amet sit dolor ipsum Lorem. Ac dolor ac adipiscing amet
 bibendum nullam, massa lacus molestie ut libero nec, diam et, pharetra sodales eget, feugiat
 ullamcorper id tempor eget id vitae. Mauris pretium eget aliquet, lectus tincidunt. Porttitor
 mollis imperdiet libero senectus pulvinar.">>,
    Encoded = hessian:encode(string,Native, State),
    {_,Decoded, State} = hessian:decode(Encoded, State),
    ?assertMatch(Native,Decoded).

object_test() ->
    Car1 = {car,<<"red">>,<<"corvette">>},
    Car2 = {car,<<"green">>,<<"civic">>},
    ForeignType = <<"example.Car">>,
    NativeType = car,
    TypeDef = #type_def{foreign_type = ForeignType,
                        native_type = NativeType,
                        fieldnames = [color,model]},
    {_,State} = type_mapping:register_type_def(TypeDef),
    % See comment in implementation to take this out
    %EncodedType = hessian:encode(string, ForeignType,[]),
    %Def = <<$O,EncodedType/binary,16#92,5,"color",5,"model">>,
    Length = hessian:encode(int, size(ForeignType), State),
    Def = <<$O,Length/binary,ForeignType/binary,16#92,5,"color",5,"model">>,
    Bin1 = <<$o,16#90,3,"red",8,"corvette">>,
    Bin2 = <<$o,16#90,5,"green",5,"civic">>,
    {_, _, NewState} = hessian:decode(Def,State),
    Ref3 = type_mapping:locate_type_reference(TypeDef, NewState),
    ?assertMatch( 0, Ref3),
    {_, Object1,_} = hessian:decode(Bin1,NewState),
    {_, Object2,_} = hessian:decode(Bin2,NewState),
    ?assertMatch(Car1,Object1),
    ?assertMatch(Car2,Object2),
    {Encoded1, State1} = hessian:encode(object, Car1, State),
    Expected1 = list_to_binary([Def] ++ [Bin1]),
    ?assertMatch(Expected1, Encoded1),
    {Encoded2, State2} = hessian:encode(object, Car2, State1),
    ?assertMatch(Bin2, Encoded2).

nested_object_test() ->
    Expected = <<%% O
                 79,
                 %% net.sf.cotton.A
                 159,110,101,116,46,115,102,46,99,111,116,116,111,110,46,65,
                 %% 2 fields
                 146,
                 %% name
                 4,110,97,109,101,
                 %% b
                 1,98,
                 %% o#0
                 111,144,
                 %% foobar
                 6,102,111,111,98,97,114,
                 %% O length net.sf.cotton.B
                 79,159,110,101,116,46,115,102,46,99,111,116,116,111,110,46,66,
                 %% 1 field
                 145,
                 %% number
                 6,110,117,109,98,101,114,
                 %% o#1
                 111,145,
                 %% 12345
                 212,48,57>>,
    ForeignTypeA = <<"net.sf.cotton.A">>,
    ForeignTypeB = <<"net.sf.cotton.B">>,
    TypeDefA = #type_def{foreign_type = ForeignTypeA,
                         native_type = a,
                         fieldnames = record_info(fields,a)},
    TypeDefB = #type_def{foreign_type = ForeignTypeB,
                         native_type = b,
                         fieldnames = record_info(fields,b)},
    {_,_State} = type_mapping:register_type_def(TypeDefA),
    {_,State} = type_mapping:register_type_def(TypeDefB, _State),
    A = #a{name = <<"foobar">>, b = #b{ number = 12345} },
    {Bin, State0} = hessian:encode(A, State),
    ?assertMatch(Expected, Bin),
    %% This test is asymmetric because decode($O,Rest/bin)
    %% will only consume the outer class definition,
    %% in normal circumstances, the calling decode/2 function will recursively
    %% consume all object definitions.
    {Rest, TypeDef, State1} = hessian:decode(Bin, State),
    ?assertMatch(TypeDefA, TypeDef),
    [Function|Arguments] = hessian:decode(<<99,2,0,109,0,1,97,Bin/binary>>, State),
    ?assertMatch([[A]], Arguments).

length_test() ->
    State = [],
    Encoded = hessian:encode(int,2,State),
    ?assertMatch( <<16#6e,Encoded/binary>>, hessian:encode(length,[0,1], State)).

untyped_list_test() ->
    State = [],
    List = [0,1],
    Encoded0 = hessian:encode(int, 0, State),
    Encoded1 = hessian:encode(int, 1, State),
    Length = hessian:encode(length, List, State),
    roundtrip(list, List, <<$V,Length/binary,Encoded0/binary,Encoded1/binary,$z>>).

mixed_list_test() ->
    State = [],
    First = 23,
    Second = <<"abc">>,
    List = [First,Second],
    Encoded0 = hessian:encode(int, First, State),
    Encoded1 = hessian:encode(string, Second, State),
    Length = hessian:encode(length, List, State),
    roundtrip(list, List, <<$V,Length/binary,Encoded0/binary,Encoded1/binary,$z>>).

%% TODO more vigorous testing on with optional lengths, types etc
typed_list_test() -> ok.
    %% This is how caucho serializes an int[]{0,1} in java
    %% 86,116,0,4,91,105,110,116,110,2,144,145,122,
%%     IntList = [2,3],
%%     IntListBin = <<$V,$t,0,7,$I,$n,$t,$e,$g,$e,$r,$l,0,0,0,2,$I,0,0,0,2,$I,0,0,0,3,$z>>,
%%     roundtrip(list, <<"Integer">>, IntList, IntListBin),
%%     StringList = [<<"foo">>,<<"bar">>],
%%     StringListBin = <<$V,$t,0,6,$S,$t,$r,$i,$n,$g,$l,0,0,0,2,$S,0,3,$f,$o,$o,$S,0,3,$b,$a,$r,$z>>,
%%     roundtrip(list, <<"String">>, StringList, StringListBin).

dictionary_decode_test() ->
    State = [],
    Bin = <<$M,16#91,16#03,$f,$e,$e,16#a0,16#03,$f,$i,$e,16#c9,0,16#03,$f,$o,$e,$z>>,
    {Dict,State} = hessian:decode(Bin,State),
    ?assertMatch( <<"fee">>, dict:fetch(1, Dict) ),
    ?assertMatch( <<"fie">>, dict:fetch(16, Dict) ),
    ?assertMatch( <<"foe">>, dict:fetch(256, Dict) ).

dictionary_test() ->
    State = [],
    Dict0 = dict:new(),
    Dict1 = dict:store(<<"fee">>, 1 , Dict0 ),
    Dict2 = dict:store(<<"fie">>, 16 , Dict1 ),
    DictN = dict:store(<<"foe">>, 256 , Dict2 ),
    Bin = hessian:encode(dictionary, DictN, State),
    {Dict,State} = hessian:decode(Bin,State),
    ?assertMatch( 1, dict:fetch(<<"fee">>, Dict) ),
    ?assertMatch( 16, dict:fetch(<<"fie">>, Dict) ),
    ?assertMatch( 256, dict:fetch(<<"foe">>, Dict) ).

%% list_reference_test() ->
%%     Native = [[<<"abc">>],[<<"abc">>]],
%%     Bin = <<$r,?M,?m,$V,$l,0,0,0,1,$S,0,3,97,98,99,$z,$R,0,0,0,0,$z>>,
%%     Decoded = hessian:decode(Bin),
%%     ?assertMatch(Native, Decoded).

%map_reference_test() ->
    %Bin = <<$r,?M,?m,$M,$t,0,10,"LinkedList",$S,0,4,"head",$I,0,0,0,1,$S,0,4,"tail",$R,0,0,0,$z>>,
    %{Rest, Decoded} = hessian:decode(Bin),
    %io:format("------> ~p / ~p~n",[Rest, Decoded]).

    %?assertMatch( 101, dict:fetch(1, Dict) ),
    %?assertMatch( 101, dict:fetch(2, Dict) ).

%---------------------------------------------------------------------------
% Error encoding / decoding
%---------------------------------------------------------------------------

service_fault_test() ->
    State = [],
    Bin = <<$r,?M,?m,$f,
            4,"code",16,"ServiceException",
            7,"message",8,"badarith",
            6,"detail",31,"Stack trace not yet implemented",
           $z>>,
    case catch 1 div 0 of
        {'EXIT', {Error, Reason}} ->
            Encoded = hessian:encode(fault, Error, Reason, State),
            ?assertMatch(Bin, Encoded);
        _ ->
            throw(error_was_not_raised)
    end,
    {error, Decoded} = hessian:decode(Bin, State),
    ?assertMatch(badarith, Decoded).

reply_fault_test() ->
    State = [],
    Bin = <<$r,?M,?m,$f,
            4,"code",16,"ServiceException",
            7,"message",19,"user_already_exists",
            6,"detail",31,"Stack trace not yet implemented",
           $z>>,
    Error = {error,{user_already_exists,<<"guest">>}},
    Encoded = hessian:encode(reply,Error, State),
    ?assertMatch(Bin, Encoded).

protocol_fault_test() ->
    State = [],
    Bin = <<$r,?M,?m,$f,
            4,"code",17,"ProtocolException",
            7,"message",24,"unexpected_byte_sequence",
            6,"detail",31,"Stack trace not yet implemented",
          $z>>,
    {error, Encoded} = hessian:decode(<<$z,77,21,33>>, State),
    ?assertMatch(Bin, Encoded),
    {error, Reason} = hessian:decode(Bin, State),
    ?assertMatch(unexpected_byte_sequence, Reason).

%---------------------------------------------------------------------------
% Longer encoding / decoding
%---------------------------------------------------------------------------

invocation_test() ->
    State = [],
    Bin = <<$c,?M,?m,$m,3:16,"add",167,168,$z>>,
    Encoded = hessian:invoke(test_module, Bin, State),
    ?assertMatch( <<$r,?M,?m,16#bf,$z>>, Encoded).

reply_test() ->
    roundtrip(reply, 10, <<$r,?M,?m,154,$z>>).

object_reply_test() ->
    Car = {car,<<"red">>,<<"corvette">>},
    ForeignType = <<"example.Car">>,
    NativeType = car,
    TypeDef = #type_def{foreign_type = ForeignType,
                        native_type = NativeType,
                        fieldnames = [color,model]},
    {_,State} = type_mapping:register_type_def(TypeDef),
    Encoded = hessian:encode(reply, {ok, Car}, State),
    Length = hessian:encode(int, size(ForeignType), State),
    Expected = <<$r,?M,?m,$O,Length/binary,ForeignType/binary,16#92,5,"color",5,"model",$o,16#90,3,"red",8,"corvette",$z>>,
    ?assertMatch(Expected, Encoded),
    Decoded = hessian:decode(Encoded, State),
    ?assertMatch(Car, Decoded).

void_reply_test() ->
    roundtrip(reply, ok, <<$r,?M,?m,$N,$z>>).

call_test() ->
    State = [],
    Function = <<"foo">>,
    Args = [1,2],
    Bin = <<$c,?M,?m,$m,3:16,"foo",145,146,122>>,
    Encoded = hessian:encode(call, Function, Args, State),
    ?assertMatch(Bin, Encoded),
    [_Function, Arguments] = hessian:decode(Bin, State),
    ?assertMatch(Function, _Function),
    ?assertMatch(Args, Arguments).

%---------------------------------------------------------------------------
% Utility methods
%---------------------------------------------------------------------------

assertDictionary(Dict) ->
    ?assertMatch( <<"foo">> , dict:fetch(<<"first">>, Dict) ),
    ?assertMatch( <<"bar">> , dict:fetch(<<"second">>, Dict) ),
    ?assertMatch( 399 , dict:fetch(<<"third">>, Dict) ).

%% TODO These expected records need to be generated based on the record
%% being passed in rather than being hardcoded
expected_record(TestRecord, State) ->
    Fqn = TestRecord#test_record.fqn,
    Encoded = hessian:encode(string, Fqn,[]),
    ExpectedList = [$M,$t,0,size(Fqn),Fqn,
                    hessian:encode(string, <<"first">>, State),
                    hessian:encode(string, <<"foo">>, State),
                    hessian:encode(string, <<"second">>, State),
                    hessian:encode(string, <<"bar">>, State),
                    hessian:encode(string, <<"third">>, State),
                    hessian:encode(int,399,State), $z],
    list_to_binary(ExpectedList).

anonymous_expected_record(State) ->
    ExpectedList = [$M,
                    hessian:encode(string, <<"first">>, State),
                    hessian:encode(string, <<"foo">>, State),
                    hessian:encode(string, <<"second">>, State),
                    hessian:encode(string, <<"bar">>, State),
                    hessian:encode(string, <<"third">>, State),
                    hessian:encode(int,399, State), $z],
    list_to_binary(ExpectedList).

record_registry() ->
    FieldNames = record_info(fields, test_record),
    Dict0 = dict:new(),
    dict:store(test_record, FieldNames, Dict0).

test_coverage() ->
    rabbit_misc:enable_cover(),
    test(),
    rabbit_misc:report_cover().

roundtrip(reply, ok, Bin) ->
    State = [],
    Encoded = hessian:encode(reply, ok, State),
    ?assertMatch(Bin, Encoded),
    ok = hessian:decode(Bin, State);

roundtrip(reply, Native, Bin) ->
    State = [],
    Encoded = hessian:encode(reply, Native, State),
    ?assertMatch(Bin, Encoded),
    Decoded = hessian:decode(Bin, State),
    ?assertMatch(Native, Decoded);

roundtrip(Type, Native, Bin) ->
    Encoded = hessian:encode(Type, Native,[]),
    ?assertMatch(Bin, Encoded),
    {Rest, Decoded, State} = hessian:decode(Bin,[]),
    ?assertMatch(Native, Decoded).

roundtrip(list, Type, NativeList, Bin) ->
    Encoded = hessian:encode(list, Type, NativeList),
    ?assertMatch(Bin, Encoded),
    {Rest, Decoded} = hessian:decode(Bin),
    ?assertMatch(NativeList, Decoded).
