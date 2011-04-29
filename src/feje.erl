-module(feje).


-on_load(start/0).

-export([start/0]).
-export([encode/1]).%, bar/1]).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

encode(_Binary) -> exit(nif_library_not_loaded).
%bar(_Y) -> exit(nif_library_not_loaded).

% decode(Binary) -> ok.
% 
% encode(Object) -> ok.

%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------


start() ->
  load_nif(erlang:system_info(otp_release) >= "R13B04").
  
load_nif(true) -> erlang:load_nif("ebin/feje", 0).