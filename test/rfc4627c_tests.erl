-module(rfc4627c_tests).


-include_lib("eunit/include/eunit.hrl").

encode1_test() ->
	
	List1	=	[],
	List2	=	lists:seq(1, 128),
	List3	=	lists:seq(1, 4096),
	List4	=	lists:seq(1, 65536),
	
	Int1	=	0,
	Int2	=	1024,
	Int3	=	1024 * 1024,
	Int4	=	1024 * 1024 * 1024,
	Int5	=	- 1024,
	Int6	=	- 1024 * 1024,
	Int7	=	- 1024 * 1024 * 1024,
	
	Float1	= 1024.3475663574,
	Float2	= Float1 * Float1,
	Float3	= Float2 * Float2,
	Float4	= Float3 * Float3,
	Float5	= Float4 * Float4,
	Float6	= 1 / Float1,
	Float7	= 1 / Float2,
	Float8	= 1 / Float3,
	Float9	= 1 / Float4,
	Float10	= 1 / Float5,
	
	Binary1 = <<>>,
	Binary2 = <<"some text 12345567890 ~!@#$%^&*()_''\"[]{}">>,
	Binary3 = <<"русский тест 中文文本 العربية κείμενο στην ελληνική γλώσσα">>,
	Binary4 = lists:foldl(fun(X, Acc) -> <<Acc/binary, Acc/binary>> end, Binary3, lists:seq(1, 10)),
	
	Obj1 = {obj, []},
	Obj2 = {obj, [{a,2}, {a,3}, {a,4}, {a,5}]},
	Obj3 = {obj, [{"a",List3}, {"b", Int4}, {"c", Binary3}, {"d", Obj2}]},
	Obj4 = {obj, lists:map(fun(X)->{integer_to_list(X), Obj3} end, lists:seq(1, 100))},
	
	F1 = fun(Object) -> rfc4627c:encode(Object) end,
	F2 = fun(Object) -> rfc4627:encode(Object) end,
	Eq = fun(Obj1, Obj2) -> Obj1 =:= list_to_binary(Obj2) end,
	
	io:format("~n~n~n", []),
	
	test([List1], F1, F2, 1, Eq, 100),
	test([List2], F1, F2, 2, Eq, 100),
	test([List3], F1, F2, 3, Eq, 100),
	test([List4], F1, F2, 4, Eq, 100),
	
	test([Int1], F1, F2, 5, Eq, 100),
	test([Int2], F1, F2, 6, Eq, 100),
	test([Int3], F1, F2, 7, Eq, 100),
	test([Int4], F1, F2, 8, Eq, 100),
	test([Int5], F1, F2, 9, Eq, 100),
	test([Int6], F1, F2, 10, Eq, 100),
	test([Int7], F1, F2, 11, Eq, 100),
	
	test([Float1], F1, F2, 12, Eq, 100),
	test([Float2], F1, F2, 13, Eq, 100),
	test([Float3], F1, F2, 14, Eq, 100),
	test([Float4], F1, F2, 15, Eq, 100),
	test([Float5], F1, F2, 16, Eq, 100),
	test([Float6], F1, F2, 17, Eq, 100),
	test([Float7], F1, F2, 18, Eq, 100),
	test([Float8], F1, F2, 19, Eq, 100),
	test([Float9], F1, F2, 20, Eq, 100),
	test([Float10], F1, F2, 21, Eq, 100),
	
	test([Binary1], F1, F2, 22, Eq, 100),
	test([Binary2], F1, F2, 23, Eq, 100),
	test([Binary3], F1, F2, 24, Eq, 100),	
	test([Binary4], F1, F2, 24, Eq, 100),
	
	test([Obj1], F1, F2, 25, Eq, 100),	
	test([Obj2], F1, F2, 26, Eq, 100),	
	test([Obj3], F1, F2, 27, Eq, 100),	
	test([Obj4], F1, F2, 28, Eq, 100),	
	
	io:format("~n~n~n", []).
		

test(Args, Func1, Func2, CaseN, Equal, TestsN) ->
	
	Data1 = lists:map(fun(_) -> timer:tc(Func1, Args) end, lists:seq(1, TestsN)),
	Data2 = lists:map(fun(_) -> timer:tc(Func2, Args) end, lists:seq(1, TestsN)),
	
	Sum1 = lists:foldl(fun({Time, _}, Acc) -> Time + Acc end, 0, Data1),
	Sum2 = lists:foldl(fun({Time, _}, Acc) -> Time + Acc end, 0, Data2),
		
	AverTime1 = Sum1 / TestsN,
	AverTime2 = Sum2 / TestsN,
	
	[{_, Result1} | _] = Data1,
	[{_, Result2} | _] = Data2,
	
	Winner = case AverTime1 < AverTime2 of
		true -> 1;
		false -> 2
	end,
	Output = [CaseN, Equal(Result1, Result2), AverTime1, AverTime2, Winner, AverTime2 / AverTime1],
	io:format("\t::test~p\t~p\t~p\t~p\t~p\t~f", Output),
	
	case Equal(Result1, Result2) of
		true -> ok;
		false -> io:format("\t~p =/= ~p\t", [Result1, Result2])
	end,
	
	io:format("~n",[]).