%%% vim: set ts=4 sts=4 sw=4 tw=79 et :
%%%---------------------------------------------------------------------------
%%% Copyright (c) 2016, Mark Jones <markalanj@gmail.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%---------------------------------------------------------------------------

-module(bmath_libc).
-author('Mark Jones <markalanj@gmail.com>').

-include("include/bmath.hrl").

%% basic operations
-export([fadd/2, fsub/2, fmul/2, fdiv/2, fabs/1, fmod/2, remainder/2, 
         remquo/2, fma/3, fmax/2, fmin/2, fdim/2]).
%% exponential functions
-export([exp/1, exp2/1, expm1/1, log/1, log10/1, log2/1, log1p/1]).
%% power functions
-export([pow/2, sqrt/1, cbrt/1, hypot/2]).
%% trigonometric functions
-export([sin/1, cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2]).
%% hyperbolic functions
-export([sinh/1, cosh/1, tanh/1, asinh/1, acosh/1, atanh/1]).
%% error and gamma functions
-export([erf/1, erfc/1, tgamma/1, lgamma/1]).
%% nearest integer floating-point operations
-export([ceil/1, floor/1, trunc/1, round/1]).
%% floating-point manipulation functions
-export([modf/1, next_after/2, copy_sign/2]).
%% classification and comparison
-export([classify/1, is_normal/1, is_gt/2, is_ge/2, is_lt/2, is_le/2,
         is_ltgt/2, is_unordered/2]).

-on_load(on_load/0).

on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      AppPath = filename:dirname(filename:dirname(
                                                   code:which(?MODULE))),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "bmath"), 0).

fadd(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fsub(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fmul(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fdiv(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fabs(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fmod(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

remainder(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

remquo(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fma(_X, _Y, _Z) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fmax(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fmin(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

fdim(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

exp(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

exp2(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

expm1(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

log(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

log10(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

log2(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

log1p(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

pow(_Base, _Exponent) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

sqrt(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cbrt(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

hypot(_X, _Y) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

sin(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cos(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

tan(_X) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

asin(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

acos(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

atan(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

atan2(_Y, _X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

sinh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cosh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

tanh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

asinh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

acosh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

atanh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

erf(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

erfc(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

tgamma(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

lgamma(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

ceil(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

floor(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

trunc(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

round(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

modf(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

next_after(_From, _To) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

copy_sign(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

classify(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_normal(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_gt(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_ge(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_lt(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_le(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_ltgt(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

is_unordered(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

