-module(rfc4627c_tests).


-include_lib("eunit/include/eunit.hrl").

encode1_test() ->
	
	% Test ints
	?assertEqual(<<"0">>, rfc4627c:encode(0)),
	?assertEqual(<<"1">>, rfc4627c:encode(1)),
	?assertEqual(<<"-1">>, rfc4627c:encode(-1)),
	?assertEqual(<<"65536">>, rfc4627c:encode(65536)),
	?assertEqual(<<"-65535">>, rfc4627c:encode(-65535)),
	?assertEqual(<<"4294967296">>, rfc4627c:encode(4294967296)),
	?assertEqual(<<"-4294967296">>, rfc4627c:encode(-4294967296)),
	
	%Floats
	?assertEqual(<<"0.00000000000000000000e+00">>, rfc4627c:encode(0.0)),
	?assertEqual(<<"1.00000000000000000000e+00">>, rfc4627c:encode(1.0)),
	?assertEqual(<<"-1.00000000000000000000e+00">>, rfc4627c:encode(-1.0)),
	?assertEqual(<<"2.36546273546724639125e-01">>, rfc4627c:encode(0.2365462735467246524237645)),
	?assertEqual(<<"-2.73476235476235451927e-01">>, rfc4627c:encode(-0.27347623547623545)),
	?assertEqual(<<"3.78643276547623557248e+28">>, rfc4627c:encode(3.7864327654762356e28)),
	?assertEqual(<<"-1.23712897636125536506e+30">>, rfc4627c:encode(-1.2371289763612554e30)),
	
	%Lists
	?assertEqual(<<"[]">>, rfc4627c:encode([])),
	?assertEqual(<<"[1,2,3]">>, rfc4627c:encode([1,2,3])),
	?assertEqual(<<"[[1],0.00000000000000000000e+00,[[]]]">>, rfc4627c:encode([[1],0.0,[[]]])),
	
	%Binaries
	?assertEqual(<<"\"\"">>, rfc4627c:encode(<<>>)),
	?assertEqual(<<"\"some text 12345567890 ~!@#$%^&*()_''\\\"[]{}\"">>, rfc4627c:encode(<<"some text 12345567890 ~!@#$%^&*()_''\"[]{}">>)),
	?assertEqual(<<"\"русский тест 中文文本 العربية κείμενο στην ελληνική γλώσσα\"">>, rfc4627c:encode(<<"русский тест 中文文本 العربية κείμενο στην ελληνική γλώσσα">>)),
	
	%Objects
	?assertEqual(<<"{}">>, rfc4627c:encode({obj, []})),
	?assertEqual(<<"{\"integer\":42,\"float\":0.00000000000000000000e+00,\"string\":\"string\"}">>, rfc4627c:encode({obj, [{integer, 42}, {"float", 0.0}, {<<"string">>, <<"string">>}]})),
	?assertEqual(<<"{\"obj\":[{}],\"array\":[1,2,3,4],\"null\":null}">>, rfc4627c:encode({obj, [{obj, [{obj, []}]}, {array, [1,2,3,4]}, {null, null}]})).