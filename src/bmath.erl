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

%%%----------------------------------------------------------------------------
%%% @author Mark Jones <markalanj@gmail.com>
%%% @copyright 2016 Mark Jones
%%% @version 0.1.0
%%% @doc 
%%% Better math library. Erlang has poor support for common floating 
%%% point operations. This library tries to improve that by providing NIFs
%%% to the C99 math library. Operations can return atoms representing 
%%% not a number and infinity. 
%%%
%%% In addition to the C99 math functions, the library provides some common
%%% math constants and other functions such as unit conversions and vector 
%%% (list) operations. Note vector functions are not currently 
%%% optimized with native code.
%%% 
%%% @end
%%%--------------------------------------------------------------------------- 

-module(bmath).
-author('Mark Jones <markalanj@gmail.com>').

-include("include/bmath.hrl").

%% basic operations
-export([fadd/2, fsub/2, fmul/2, fdiv/2, fabs/1, fmod/2, remainder/2, 
         remquo/2, fma/3, fmax/2, fmin/2, fdim/2, bound/3]).
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
-export([classify/1, is_finite/1, is_inf/1, is_nan/1, is_normal/1,
         is_gt/2, is_ge/2, is_lt/2, is_le/2, is_ltgt/2, is_unordered/2,
         fuzzy_compare/2, fuzzy_zero/1]).
%% conversions
-export([degrees_to_radians/1, radians_to_degrees/1, nan_to_num/1]).
%% statistics 
-export([sum/1, mean/1, variance/1, stddev/1]).

-on_load(on_load/0).

-type nan() :: nan | '-nan'.
-type infinity() :: inf | '-inf'.

-export_type([nan/0, infinity/0]).

%%====================================================================
%% API
%%====================================================================

on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      AppPath = filename:dirname(filename:dirname(
                                                   code:which(?MODULE))),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

-spec fadd(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc X + Y

fadd(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fsub(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc X - Y 

fsub(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fmul(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc X * Y 

fmul(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fdiv(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc X / Y 

fdiv(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fabs(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes the absolute value of X

fabs(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fmod(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Computes the floating-point remainder of the division operation X / Y

fmod(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec remainder(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Computes signed remainder of the floating-point division operation 

remainder(_X, _Y_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec remquo(_X, _Y) -> {float() | nan() | infinity(), integer()} when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Computes signed remainder as well as the three last bits of the 
%% division operation 

remquo(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fma(_X, _Y, _Z) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity(),
      _X :: number() | nan() | infinity().
%% @doc Computes fused multiply-add operation

fma(_X, _Y, _Z) -> 
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fmax(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Determines larger of two floating-point values.

fmax(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fmin(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Determines smaller of two floating-point values  

fmin(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fdim(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Determines positive difference of two floating-point values 
%% (max(0, x-y)) 

fdim(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @doc Returns Value bounded by Min and Max

bound(Value, Min, Max) ->
    fmax(Min, fmin(Max, Value)).

-spec exp(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes e raised to the given power

exp(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec exp2(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes 2 raised to the given power

exp2(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec expm1(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes e raised to the given power, minus one

expm1(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec log(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes natural (base-e) logarithm

log(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec log10(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes the common (base-10) logarithm

log10(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec log2(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes the base-2 logarithm

log2(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec log1p(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes the (base-e) logarithm of 1 plus the given number

log1p(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec pow(_Base, _Exponent) -> float() | nan() | infinity() when
      _Base :: number() | nan() | infinity(),
      _Exponent :: number() | nan() | infinity().
%% @doc Computes a number raised to the given power

pow(_Base, _Exponent) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec sqrt(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes square root

sqrt(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec cbrt(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes cubic root

cbrt(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec hypot(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Computes the square root of the sum of the squares of two given
%% numbers.

hypot(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec sin(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes sine

sin(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec cos(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes cosine

cos(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec tan(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes tangent

tan(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec asin(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes arc sine

asin(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec acos(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes arc cosine

acos(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atan(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes arc tangent

atan(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atan2(_Y, _X) -> float() | nan() | infinity() when
      _Y :: number() | nan() | infinity(),
      _X :: number() | nan() | infinity().
%% @doc Computes arc tangent, using signs to determine quadrants

atan2(_Y, _X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec sinh(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes hyperbolic sine 

sinh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec cosh(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes hyperbolic cosine 

cosh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec tanh(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes hyperbolic tangent 

tanh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec asinh(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes inverse hyperbolic sine 

asinh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec acosh(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes inverse hyperbolic cosine 

acosh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atanh(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes inverse hyperbolic tangent 

atanh(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec erf(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes error function

erf(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec erfc(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes complementary error function

erfc(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec tgamma(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes gamma function. 

tgamma(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec lgamma(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes natural (base-e) logarithm of the gamma function. 

lgamma(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec ceil(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes smallest integer not less than the given value. 

ceil(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec floor(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes largest integer not greater than the given value. 

floor(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec trunc(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Rounds to the nearest integer not greater in magnitude than the given
%% value. 

trunc(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec round(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Rounds to the nearest integer, rounding away from zero in halfway 
%% cases.

round(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec modf(_X) -> {float() | nan() | infinity(), float() | nan} when
      _X :: number() | nan() | infinity().
%% @doc Breaks a number into integer and fractional parts

modf(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec next_after(_From, _To) -> float() | nan() | infinity() when
      _From :: number() | nan() | infinity(),
      _To :: number() | nan() | infinity().
%% @doc Determines next representable floating-point value towards the given 
%% value 

next_after(_From, _To) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec copy_sign(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Produces a value with the magnitude of a given value and the sign of
%% another given value 

copy_sign(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec classify(_X) -> inf | nan | normal | subnormal | zero | unknown when
      _X :: number() | nan() | infinity().
%% @doc Classifies the given floating-point value 

classify(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @doc Checks if the given value is finite
is_finite(X) when is_float(X); is_integer(X) -> true;
is_finite(X) when ?BMATH_NOTFINITE(X)        -> false.

%% @doc Checks if the given value is infinite
is_inf(X) when ?BMATH_ISINF(X)                             -> true;
is_inf(X) when is_float(X); is_integer(X); ?BMATH_ISNAN(X) -> false.

%% @doc Checks if the given value is NaN 
is_nan(X) when ?BMATH_ISNAN(X)                             -> true;
is_nan(X) when is_float(X); is_integer(X); ?BMATH_ISINF(X) -> false.

-spec is_normal(_X) -> boolean() when _X :: number() | nan() | infinity().
%% @doc Checks if the given number is normal.

is_normal(_X) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_gt(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is greater than the second 

is_gt(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_ge(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is greater or equal than
%% the second 

is_ge(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_lt(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is less than the second 

is_lt(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_le(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is less or equal than 
%% the second 

is_le(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_ltgt(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is less or greater than 
%% the second 

is_ltgt(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_unordered(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if two floating-point values are unordered

is_unordered(_X, _Y) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec fuzzy_compare(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Compares two floats in a relative way to see if they are equal. 

fuzzy_compare(X, Y) ->
    is_le(fmul(fabs(fsub(X,Y)),1000000000000.0),fmin(fabs(X),fabs(Y))).

-spec fuzzy_zero(X) -> boolean() when
      X :: number() | nan() | infinity().
%% @doc Checks if a number is condisered zero. 

fuzzy_zero(X) ->
    is_le(fabs(X), 0.000000000001).

%% @doc Converts degrees to radians

degrees_to_radians(XS) when is_list(XS) ->
    degrees_to_radians(XS, []);
degrees_to_radians(X)                   -> 
    fmul(X, ?BMATH_PI/180).

%% @doc Converts radians to degrees

radians_to_degrees(XS) when is_list(XS) ->
    radians_to_degrees(XS, []);
radians_to_degrees(X)                   -> 
    fmul(X, 180/?BMATH_PI).

nan_to_num(X) when is_number(X)        -> X;
nan_to_num(X) when ?BMATH_NOTFINITE(X) -> 0;
nan_to_num(X) when is_list(X)          ->
    nan_to_num(X,[]).

%% @doc Computes the sum of a list of numbers. Numbers which are not finite
%% are skipped and have no affect on the result.

sum([]) -> 0;
sum(XS) ->
    sum(XS, 0).

%% @doc Computes the mean of a list of numbers. Numbers which are not finite
%% are skipped and have no affect on the result.

mean([]) -> 0;
mean(XS) ->
    mean(XS, {0,0}).

%% @doc Computes the variance of a list of numbers. Numbers which are not
%% finite are skipped and have no affect on the result.

variance([]) -> 0;
variance(XS) ->
    Mean = mean(XS),
    F = fun(X, {Len, Sum}) -> 
                case is_finite(X) of
                    true ->
                        {Len + 1, Sum + pow(X - Mean, 2)};
                    false ->
                        {Len, Sum} % skip non finite values
                end
        end,
    {Len, Sum} = lists:foldl(F, {0,0}, XS),
    Sum / Len.

%% @doc Computes the standard devation of a list of numbers. Numbers which are
%% not finite are skipped and have no affect on the result.

stddev([]) -> 0;
stddev(XS) ->
    sqrt(variance(XS)).

%%====================================================================
%% Internal functions
%%====================================================================

degrees_to_radians([], Acc)     ->
    lists:reverse(Acc);
degrees_to_radians([X|XS], Acc) ->
    degrees_to_radians(XS, [degrees_to_radians(X) | Acc]).

radians_to_degrees([], Acc)     ->
    lists:reverse(Acc);
radians_to_degrees([X|XS], Acc) ->
    radians_to_degrees(XS, [radians_to_degrees(X) | Acc]).

nan_to_num([], Acc)     ->
    lists:reverse(Acc);
nan_to_num([X|XS], Acc) ->
    nan_to_num(XS, [nan_to_num(X) | Acc]).  

sum([], Acc)                              ->
    Acc;
sum([X|XS], Acc) when is_number(X)        ->
    sum(XS, X + Acc);
sum([X|XS], Acc) when ?BMATH_NOTFINITE(X) ->
    sum(XS, Acc).            % skip non finite values

mean([], {Sum, Len})                              -> 
    Sum / Len;
mean([X|XS], {Sum, Len}) when is_number(X)        ->
    mean(XS, {X + Sum, Len + 1});
mean([X|XS], {Sum, Len}) when ?BMATH_NOTFINITE(X) ->
    mean(XS, {Sum, Len}).    % skip non finite values

