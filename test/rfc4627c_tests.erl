-module(rfc4627c_tests).


-include_lib("eunit/include/eunit.hrl").


encode1_test() ->
	
	List1	=	[],
	List2	=	lists:seq(1, 32 * 32),
	List3	=	lists:seq(1, 32 * 32 * 32),
	List4	=	lists:seq(1, 32 * 32 * 32 * 32),
	
	Int1	=	0,
	Int2	=	1024,
	Int3	=	1024 * 1024,
	Int4	=	1024 * 1024,
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
	
	% Binary1 = <<>>,
	% Binary2 = <<"some text 12345567890 ~!@#$%^&*()_">>,
	% Binary3 = <<[1,2,3]>>,
	% Binary4 = <<"русский тест \\ud834\\udd20">>,
	
	rfc4627c:encode(List1),
	rfc4627c:encode(List2),
	% rfc4627c:encode(List3),
	% rfc4627c:encode(List4),
	
	ok.
	
	
    % CD = iconverl:open("ucs-2be", "utf-8"),
    % ?assertEqual(
    %     {ok, <<0,$t,0,$e,0,$s,0,$t>>},
    %     iconverl:conv(CD, <<"test">>)
    % ),
    % ?assertEqual(
    %     {error, eilseq},
    %     iconverl:conv(CD, <<129,129>>)
    % ).