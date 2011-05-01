%% JSON - RFC 4627 ecoder/decoder - for Erlang
%%---------------------------------------------------------------------------
%% @author Vladimir Zaytsev <vladimir@zvm.me>
%% @copyright (C) 2011 Vladimir Zaystev
%% @license
%%
%% Permission is hereby granted, free of charge, to any author
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit authors to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
-module(rfc4627c).
-author('Vladimir Zaytsev <vladimir@zvm.me>').

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