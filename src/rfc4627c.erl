-module(rfc4627c).


-on_load(start/0).

-export([start/0]).
-export([encode/1]).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec encode/1 :: ({'object', list()} | list() | integer() | binary() | float() | 'null') -> binary().
encode(_Object) -> exit(nif_library_not_loaded).

%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------


start() ->
  load_nif(erlang:system_info(otp_release) >= "R13B04").
  
load_nif(true) -> erlang:load_nif("ebin/rfc4627c", 0).