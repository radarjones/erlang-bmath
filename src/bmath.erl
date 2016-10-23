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
%%% math constants and other functions such as unit conversions and statstics 
%%% operations. 
%%% 
%%% @end
%%%--------------------------------------------------------------------------- 

-module(bmath).
-author('Mark Jones <markalanj@gmail.com>').

-include("include/bmath.hrl").

-export([fadd/2, fsub/2, fmul/2, fdiv/2, fabs/1, fmod/2, remainder/2,
         remquo/2, fma/3, fmax/2, fmin/2, bound/3, fdim/2, exp/1, exp2/1,
         expm1/1, log/1, log10/1, log2/1, log1p/1, pow/2, sqrt/1, cbrt/1,
         hypot/2, sin/1, cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2, 
         sinh/1, cosh/1, tanh/1, asinh/1, acosh/1, atanh/1, erf/1, erfc/1,
         tgamma/1, lgamma/1, ceil/1, floor/1, trunc/1, round/1, modf/1,
         next_after/2, copy_sign/2, classify/1, is_normal/1, is_gt/2,
         is_ge/2, is_lt/2, is_le/2, is_ltgt/2, is_unordered/2,
         is_finite/1, is_infinity/1, is_nan/1, fuzzy_compare/2, fuzzy_zero/1,
         degrees_to_radians/1, radians_to_degrees/1, nan_to_num/1, sum/1,
         mean/1, variance/1, stddev/1]).

-type nan() :: nan | '-nan'.
-type infinity() :: inf | '-inf'.

-export_type([nan/0, infinity/0]).

-spec fadd(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc X + Y

fadd(X,Y) -> 
    bmath_libc:fadd(X,Y).

-spec fsub(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc X - Y 

fsub(X, Y) -> 
    bmath_libc:fsub(X, Y).

-spec fmul(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc X * Y 

fmul(X, Y) -> 
    bmath_libc:fmul(X, Y).

-spec fdiv(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc X / Y 

fdiv(X, Y) -> 
    bmath_libc:fdiv(X, Y).

-spec fabs(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes the absolute value of X

fabs(X) -> 
    bmath_libc:fabs(X).

-spec fmod(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Computes the floating-point remainder of the division operation X / Y

fmod(X, Y) -> 
    bmath_libc:fmod(X, Y).

-spec remainder(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Computes signed remainder of the floating-point division operation 

remainder(X, Y) -> 
    bmath_libc:remainder(X, Y).

-spec remquo(X, Y) -> {float() | nan() | infinity(), integer()} when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Computes signed remainder as well as the three last bits of the 
%% division operation 

remquo(X, Y) -> 
    bmath_libc:remquo(X, Y).

-spec fma(X, Y, Z) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity(),
      Z :: number() | nan() | infinity().
%% @doc Computes fused multiply-add operation

fma(X, Y, Z) -> 
    bmath_libc:fma(X, Y, Z).

-spec fmax(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Determines larger of two floating-point values.

fmax(X, Y) -> 
    bmath_libc:fmax(X, Y).

-spec fmin(_X, _Y) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Determines smaller of two floating-point values  

fmin(X, Y) -> 
    bmath_libc:fmin(X, Y).

%% @doc Returns Value bounded by Min and Max
bound(Value, Min, Max) -> 
    fmax(Min, fmin(Max, Value)).

-spec fdim(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Determines positive difference of two floating-point values 
%% (max(0, x-y)) 

fdim(X, Y) -> 
    bmath_libc:fdim(X, Y).

-spec exp(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes e raised to the given power

exp(X) ->
    bmath_libc:exp(X).

-spec exp2(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes 2 raised to the given power

exp2(X) ->
    bmath_libc:exp2(X).

-spec expm1(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes e raised to the given power, minus one

expm1(X) ->
    bmath_libc:expm1(X).

-spec log(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes natural (base-e) logarithm

log(X) ->
    bmath_libc:log(X).

-spec log10(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes the common (base-10) logarithm

log10(X) ->
    bmath_libc:log10(X).

-spec log2(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes the base-2 logarithm

log2(X) ->
    bmath_libc:log2(X).

-spec log1p(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes the (base-e) logarithm of 1 plus the given number

log1p(X) ->
    bmath_libc:log1p(X).

-spec pow(Base, Exponent) -> float() | nan() | infinity() when
      Base :: number() | nan() | infinity(),
      Exponent :: number() | nan() | infinity().
%% @doc Computes a number raised to the given power

pow(Base, Exponent) ->
    bmath_libc:pow(Base, Exponent).

-spec sqrt(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes square root

sqrt(X) ->
    bmath_libc:sqrt(X).

-spec cbrt(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes cubic root

cbrt(X) ->
    bmath_libc:cbrt(X).

-spec hypot(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Computes the square root of the sum of the squares of two given
%% numbers.

hypot(X, Y) ->
    bmath_libc:hypot(X, Y).

-spec sin(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes sine

sin(X) ->
    bmath_libc:sin(X).

-spec cos(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes cosine

cos(X) ->
    bmath_libc:cos(X).

-spec tan(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes tangent

tan(X) ->
    bmath_libc:tan(X).

-spec asin(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes arc sine

asin(X) ->
    bmath_libc:asin(X).

-spec acos(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes arc cosine

acos(X) ->
    bmath_libc:acos(X).

-spec atan(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes arc tangent

atan(X) ->
    bmath_libc:atan(X).

-spec atan2(Y, X) -> float() | nan() | infinity() when
      Y :: number() | nan() | infinity(),
      X :: number() | nan() | infinity().
%% @doc Computes arc tangent, using signs to determine quadrants

atan2(Y, X) ->
    bmath_libc:atan2(Y, X).

-spec sinh(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes hyperbolic sine 

sinh(X) ->
    bmath_libc:sinh(X).

-spec cosh(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes hyperbolic cosine 

cosh(X) ->
    bmath_libc:cosh(X).

-spec tanh(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes hyperbolic tangent 

tanh(X) ->
    bmath_libc:tanh(X).

-spec asinh(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes inverse hyperbolic sine 

asinh(X) ->
    bmath_libc:asinh(X).

-spec acosh(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes inverse hyperbolic cosine 

acosh(X) ->
    bmath_libc:acosh(X).

-spec atanh(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes inverse hyperbolic tangent 

atanh(X) ->
    bmath_libc:atanh(X).

-spec erf(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes error function

erf(X) ->
    bmath_libc:erf(X).

-spec erfc(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes complementary error function

erfc(X) ->
    bmath_libc:erfc(X).

-spec tgamma(_X) -> float() | nan() | infinity() when
      _X :: number() | nan() | infinity().
%% @doc Computes gamma function. 

tgamma(X) ->
    bmath_libc:tgamma(X).

-spec lgamma(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes natural (base-e) logarithm of the gamma function. 

lgamma(X) ->
    bmath_libc:lgamma(X).

-spec ceil(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes smallest integer not less than the given value. 

ceil(X) ->
    bmath_libc:ceil(X).

-spec floor(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Computes largest integer not greater than the given value. 

floor(X) ->
    bmath_libc:floor(X).

-spec trunc(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Rounds to the nearest integer not greater in magnitude than the given
%% value. 

trunc(X) ->
    bmath_libc:trunc(X).

-spec round(X) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity().
%% @doc Rounds to the nearest integer, rounding away from zero in halfway 
%% cases.

round(X) ->
    bmath_libc:round(X).

-spec modf(X) -> {float() | nan() | infinity(), float() | nan} when
      X :: number() | nan() | infinity().
%% @doc Breaks a number into integer and fractional parts

modf(X) ->
    bmath_libc:modf(X).

-spec next_after(From, To) -> float() | nan() | infinity() when
      From :: number() | nan() | infinity(),
      To :: number() | nan() | infinity().
%% @doc Determines next representable floating-point value towards the given 
%% value 

next_after(From, To) ->
    bmath_libc:next_after(From, To).

-spec copy_sign(X, Y) -> float() | nan() | infinity() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Produces a value with the magnitude of a given value and the sign of
%% another given value 

copy_sign(X, Y) ->
    bmath_libc:copy_sign(X, Y).

-spec classify(X) -> inf | nan | normal | subnormal | zero | unknown when
      X :: number() | nan() | infinity().
%% @doc Classifies the given floating-point value 

classify(X) ->
    bmath_libc:classify(X).

-spec is_normal(X) -> boolean() when 
      X :: number() | nan() | infinity().
%% @doc Checks if the given number is normal.

is_normal(X) ->
    bmath_libc:is_normal(X).

-spec is_gt(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is greater than the second 

is_gt(X, Y) ->
    bmath_libc:is_gt(X, Y).

-spec is_ge(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is greater or equal than
%% the second 

is_ge(X, Y) ->
    bmath_libc:is_ge(X, Y).

-spec is_lt(_X, _Y) -> boolean() when
      _X :: number() | nan() | infinity(),
      _Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is less than the second 

is_lt(X, Y) ->
    bmath_libc:is_lt(X, Y).

-spec is_le(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is less or equal than 
%% the second 

is_le(X, Y) ->
    bmath_libc:is_le(X, Y).

-spec is_ltgt(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Checks if the first floating-point argument is less or greater than 
%% the second 

is_ltgt(X, Y) ->
    bmath_libc:is_ltgt(X, Y).

-spec is_unordered(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Checks if two floating-point values are unordered

is_unordered(X, Y) ->
    bmath_libc:is_unordered(X, Y).

%% @doc Checks if the given value is finite
is_finite(X) when is_number(X)        -> true;
is_finite(X) when ?BMATH_NOTFINITE(X) -> false.

%% @doc Checks if the given value is infinite
is_infinity(X) when ?BMATH_ISINF(X)               -> true;
is_infinity(X) when is_number(X); ?BMATH_ISNAN(X) -> false.

%% @doc Checks if the given value is NaN 
is_nan(X) when ?BMATH_ISNAN(X)               -> true;
is_nan(X) when is_number(X); ?BMATH_ISINF(X) -> false.

-spec fuzzy_compare(X, Y) -> boolean() when
      X :: number() | nan() | infinity(),
      Y :: number() | nan() | infinity().
%% @doc Compares two floats in a relative way to see if they are equal. 

fuzzy_compare(X, Y) ->
    bmath_libc:is_le(
      bmath_libc:fmul(bmath_libc:fabs(bmath_libc:fsub(X,Y)),1000000000000.0),
      bmath_libc:fmin(bmath_libc:fabs(X),bmath_libc:fabs(Y))).

-spec fuzzy_zero(X) -> boolean() when
      X :: number() | nan() | infinity().
%% @doc Checks if a number is condisered zero. 

fuzzy_zero(X) ->
    bmath_libc:is_le(fabs(X), 0.000000000001).

%% @doc Converts degrees to radians
degrees_to_radians(X) -> 
    bmath_libc:fmul(X, ?BMATH_PI/180).

%% @doc Converts radians to degrees
radians_to_degrees(X) -> 
    bmath_libc:fmul(X, 180/?BMATH_PI).

nan_to_num(X) when is_number(X)        -> X;
nan_to_num(X) when ?BMATH_NOTFINITE(X) -> 0;
nan_to_num(Xs) when is_list(Xs)        ->
    nan_to_num(Xs,[]).

nan_to_num([], Acc)     ->
    lists:reverse(Acc);
nan_to_num([X|Xs], Acc) ->
    nan_to_num(Xs, [nan_to_num(X) | Acc]).  

%% @doc Computes the sum of a list of numbers. Numbers which are not finite
%% are skipped and have no affect on the result.
sum([]) -> 0;
sum(Xs) ->
    F = fun(X, Acc) ->
                case is_finite(X) of 
                    true ->
                        Acc + X;
                    false ->
                        Acc % skip non finite values
                end
        end,
    lists:foldl(F, 0, Xs).

%% @doc Computes the mean of a list of numbers. Numbers which are not finite
%% are skipped and have no affect on the result.

mean([]) -> 0;
mean(Xs) ->
    F = fun(X, {Len, Sum}) ->
                case is_finite(X) of
                    true ->
                        {Len + 1, Sum + X};
                    false ->
                        {Sum, Len} % skip non finite values
                end
        end,
    {Len, Sum} = lists:foldl(F, {0,0}, Xs),
    Sum / Len.

%% @doc Computes the variance of a list of numbers. Numbers which are not
%% finite are skipped and have no affect on the result.

variance([]) -> 0;
variance(Xs) ->
    Mean = mean(Xs),
    F = fun(X, {Len, Sum}) -> 
                case is_finite(X) of
                    true ->
                        {Len + 1, Sum + bmath_libc:pow(X - Mean, 2)};
                    false ->
                        {Len, Sum} % skip non finite values
                end
        end,
    {Len, Sum} = lists:foldl(F, {0,0}, Xs),
    Sum / Len.

%% @doc Computes the standard devation of a list of numbers. Numbers which are
%% not finite are skipped and have no affect on the result.

stddev([]) -> 0;
stddev(Xs) ->
    sqrt(variance(Xs)).

