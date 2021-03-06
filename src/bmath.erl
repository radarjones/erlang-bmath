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
         mean/1, variance/1, stddev/1, addc/2, subc/2, mulc/2, divc/2]).

-type nan() :: nan | '-nan'.
-type infinity() :: inf | '-inf'.
-type num() :: number() | nan() | infinity().

-export_type([nan/0, infinity/0, num/0]).

-spec fadd(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc X + Y

fadd(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fadd(Xs, Ys, []);
fadd(X,Y)                                  -> 
    bmath_libc:fadd(X,Y).

fadd([], [], Acc)         ->
    lists:reverse(Acc);
fadd([X|Xs], [Y|Ys], Acc) ->
    fadd(Xs, Ys, [bmath_libc:fadd(X, Y) | Acc]).

-spec fsub(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc X - Y 

fsub(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fsub(Xs, Ys, []);
fsub(X, Y)                                 -> 
    bmath_libc:fsub(X, Y).

fsub([], [], Acc)         ->
    lists:reverse(Acc);
fsub([X|Xs], [Y|Ys], Acc) ->
    fsub(Xs, Ys, [bmath_libc:fsub(X, Y) | Acc]).

-spec fmul(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc X * Y 

fmul(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fmul(Xs, Ys, []);
fmul(X, Y)                                 -> 
    bmath_libc:fmul(X, Y).

fmul([], [], Acc)         ->
    lists:reverse(Acc);
fmul([X|Xs], [Y|Ys], Acc) ->
    fmul(Xs, Ys, [bmath_libc:fmul(X, Y) | Acc]).

-spec fdiv(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc X / Y 

fdiv(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fdiv(Xs, Ys, []);
fdiv(X, Y) -> 
    bmath_libc:fdiv(X, Y).

fdiv([], [], Acc)         ->
    lists:reverse(Acc);
fdiv([X|Xs], [Y|Ys], Acc) ->
    fdiv(Xs, Ys, [bmath_libc:fdiv(X, Y) | Acc]).

-spec fabs(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes the absolute value of X

fabs(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:fabs(X) end, Xs);
fabs(X)                   -> 
    bmath_libc:fabs(X).

-spec fmod(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Computes the floating-point remainder of the division operation X / Y

fmod(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fmod(Xs, Ys, []);
fmod(X, Y)                                 -> 
    bmath_libc:fmod(X, Y).

fmod([], [], Acc)         ->
    lists:reverse(Acc);
fmod([X|Xs], [Y|Ys], Acc) ->
    fmod(Xs, Ys, [bmath_libc:fmod(X, Y) | Acc]).

-spec remainder(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Computes signed remainder of the floating-point division operation 

remainder(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    remainder(Xs, Ys, []);
remainder(X, Y)                                 -> 
    bmath_libc:remainder(X, Y).

remainder([], [], Acc)         ->
    lists:reverse(Acc);
remainder([X|Xs], [Y|Ys], Acc) ->
    remainder(Xs, Ys, [bmath_libc:remainder(X, Y) | Acc]).

-spec remquo(X, Y) -> {num(), integer()} | [{num(), integer()}]   when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Computes signed remainder as well as the three last bits of the 
%% division operation 

remquo(Xs, Ys) when is_list(Xs), is_list(Ys) ->
   remquo(Xs, Ys, []); 
remquo(X, Y)                                 -> 
    bmath_libc:remquo(X, Y).

remquo([], [], Acc)         ->
    lists:reverse(Acc);
remquo([X|Xs], [Y|Ys], Acc) ->
    remquo(Xs, Ys, [bmath_libc:remquo(X, Y) | Acc]). 

-spec fma(X, Y, Z) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()],
      Z :: num() | [num()].
%% @doc Computes fused multiply-add operation

fma(Xs, Ys, Zs) when is_list(Xs), is_list(Ys), is_list(Zs) ->
    fma(Xs, Ys, Zs, []);
fma(X, Y, Z)                                               -> 
    bmath_libc:fma(X, Y, Z).

fma([], [], [], Acc)             ->
    lists:reverse(Acc);
fma([X|Xs], [Y|Ys], [Z|Zs], Acc) ->
    fma(Xs, Ys, Zs, [bmath_libc:fma(X, Y, Z) | Acc]).

-spec fmax(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Determines larger of two floating-point values.

fmax(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fmax(Xs, Ys, []);
fmax(X, Y) -> 
    bmath_libc:fmax(X, Y).

fmax([], [], Acc)         ->
    lists:reverse(Acc);
fmax([X|Xs], [Y|Ys], Acc) ->
    fmax(Xs, Ys, [bmath_libc:fmax(X, Y) | Acc]).

-spec fmin(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Determines smaller of two floating-point values  

fmin(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fmin(Xs, Ys, []);
fmin(X, Y)                                 -> 
    bmath_libc:fmin(X, Y).

fmin([], [], Acc)         ->
    lists:reverse(Acc);
fmin([X|Xs], [Y|Ys], Acc) ->
    fmin(Xs, Ys, [bmath_libc:fmin(X, Y) | Acc]).

%% @doc Returns Value bounded by Min and Max
bound(Values, Mins, Maxs) when is_list(Values), is_list(Mins), is_list(Maxs) ->
    bound(Values, Mins, Maxs, []);
bound(Value, Min, Max)                                                       -> 
    fmax(Min, fmin(Max, Value)).

bound([], [], [], Acc)                             ->
    lists:reverse(Acc);
bound([Value|Values], [Min|Mins], [Max|Maxs], Acc) ->
    bound(Values, Mins, Maxs, [bound(Value, Min, Max) | Acc]).

-spec fdim(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Determines positive difference of two floating-point values 
%% (max(0, x-y)) 

fdim(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    fdim(Xs, Ys, []);
fdim(X, Y) -> 
    bmath_libc:fdim(X, Y).

fdim([], [], Acc)         ->
    lists:reverse(Acc);
fdim([X|Xs], [Y|Ys], Acc) ->
    fdim(Xs, Ys, [bmath_libc:fdim(X, Y) | Acc]).

-spec exp(X) -> num()  | [num()] when
      X :: num() | [num()].
%% @doc Computes e raised to the given power

exp(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:exp(X) end, Xs);
exp(X)                   ->
    bmath_libc:exp(X).

-spec exp2(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes 2 raised to the given power

exp2(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:exp2(X) end, Xs);
exp2(X)                   ->
    bmath_libc:exp2(X).

-spec expm1(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes e raised to the given power, minus one

expm1(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:expm1(X) end, Xs);
expm1(X)                   ->
    bmath_libc:expm1(X).

-spec log(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes natural (base-e) logarithm

log(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:log(X) end, Xs);
log(X)                   ->
    bmath_libc:log(X).

-spec log10(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes the common (base-10) logarithm

log10(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:log10(X) end, Xs);
log10(X)                   ->
    bmath_libc:log10(X).

-spec log2(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes the base-2 logarithm

log2(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:log2(X) end, Xs);
log2(X)                   ->
    bmath_libc:log2(X).

-spec log1p(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes the (base-e) logarithm of 1 plus the given number

log1p(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:log1p(X) end, Xs);
log1p(X)                   ->
    bmath_libc:log1p(X).

-spec pow(Base, Exponent) -> num() | [num()] when
      Base :: num() | [num()],
      Exponent :: num() | [num()].
%% @doc Computes a number raised to the given power

pow(Bases, Exponents) when is_list(Bases), is_list(Exponents) ->
    pow(Bases, Exponents, []);
pow(Base, Exponent)                                           ->
    bmath_libc:pow(Base, Exponent).

pow([], [], Acc)                             ->
    lists:reverse(Acc);
pow([Base|Bases], [Exponent|Exponents], Acc) ->
    pow(Bases, Exponents, [bmath_libc:pow(Base, Exponent) | Acc]).

-spec sqrt(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes square root

sqrt(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:sqrt(X) end, Xs);
sqrt(X)                   ->
    bmath_libc:sqrt(X).

-spec cbrt(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes cubic root

cbrt(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:cbrt(X) end, Xs);
cbrt(X)                   ->
    bmath_libc:cbrt(X).

-spec hypot(X, Y) -> num() | [num()] when
      X :: num() | [num()],
      Y :: num() | [num()].
%% @doc Computes the square root of the sum of the squares of two given
%% numbers.

hypot(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    hypot(Xs, Ys, []);
hypot(X, Y)                                 ->
    bmath_libc:hypot(X, Y).

hypot([], [], Acc)         ->
    lists:reverse(Acc);
hypot([X|Xs], [Y|Ys], Acc) ->
    hypot(Xs, Ys, [bmath_libc:hypot(X, Y) | Acc]).

-spec sin(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes sine

sin(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:sin(X) end, Xs);
sin(X)                   ->
    bmath_libc:sin(X).

-spec cos(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes cosine

cos(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:cos(X) end, Xs);
cos(X)                   ->
    bmath_libc:cos(X).

-spec tan(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes tangent

tan(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:tan(X) end, Xs);
tan(X)                   ->
    bmath_libc:tan(X).

-spec asin(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes arc sine

asin(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:asin(X) end, Xs);
asin(X)                   ->
    bmath_libc:asin(X).

-spec acos(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes arc cosine

acos(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:acos(X) end, Xs);
acos(X)                   ->
    bmath_libc:acos(X).

-spec atan(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes arc tangent

atan(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:atan(X) end, Xs);
atan(X)                   ->
    bmath_libc:atan(X).

-spec atan2(Y, X) -> num() | [num()] when
      Y :: num() | [num()],
      X :: num() | [num()].
%% @doc Computes arc tangent, using signs to determine quadrants

atan2(Ys, Xs) when is_list(Ys), is_list(Xs) ->
    atan2(Ys, Xs, []);
atan2(Y, X)                                 ->
    bmath_libc:atan2(Y, X).

atan2([], [], Acc)         ->
    lists:reverse(Acc);
atan2([Y|Ys], [X|Xs], Acc) ->
    atan2(Ys, Xs, [bmath_libc:atan2(Y, X) | Acc]).

-spec sinh(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes hyperbolic sine 

sinh(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:sinh(X) end, Xs);
sinh(X)                   ->
    bmath_libc:sinh(X).

-spec cosh(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes hyperbolic cosine 

cosh(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:cosh(X) end, Xs);
cosh(X) ->
    bmath_libc:cosh(X).

-spec tanh(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes hyperbolic tangent 

tanh(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:tanh(X) end, Xs);
tanh(X) ->
    bmath_libc:tanh(X).

-spec asinh(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes inverse hyperbolic sine 

asinh(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:asinh(X) end, Xs);
asinh(X)                   ->
    bmath_libc:asinh(X).

-spec acosh(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes inverse hyperbolic cosine 

acosh(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:acosh(X) end, Xs);
acosh(X)                   ->
    bmath_libc:acosh(X).

-spec atanh(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes inverse hyperbolic tangent 

atanh(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:atanh(X) end, Xs);
atanh(X)                   ->
    bmath_libc:atanh(X).

-spec erf(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes error function

erf(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:erf(X) end, Xs);
erf(X)                   ->
    bmath_libc:erf(X).

-spec erfc(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes complementary error function

erfc(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:erfc(X) end, Xs);
erfc(X) ->
    bmath_libc:erfc(X).

-spec tgamma(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes gamma function. 

tgamma(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:tgamma(X) end, Xs);
tgamma(X)                   ->
    bmath_libc:tgamma(X).

-spec lgamma(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes natural (base-e) logarithm of the gamma function. 

lgamma(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:lgamma(X) end, Xs);
lgamma(X)                   ->
    bmath_libc:lgamma(X).

-spec ceil(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes smallest integer not less than the given value. 

ceil(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:ceil(X) end, Xs);
ceil(X)                   ->
    bmath_libc:ceil(X).

-spec floor(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Computes largest integer not greater than the given value. 

floor(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:floor(X) end, Xs);
floor(X)                   ->
    bmath_libc:floor(X).

-spec trunc(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Rounds to the nearest integer not greater in magnitude than the given
%% value. 

trunc(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:trunc(X) end, Xs);
trunc(X)                   ->
    bmath_libc:trunc(X).

-spec round(X) -> num() | [num()] when
      X :: num() | [num()].
%% @doc Rounds to the nearest integer, rounding away from zero in halfway 
%% cases.

round(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:round(X) end, Xs);
round(X)                   ->
    bmath_libc:round(X).

-spec modf(X) -> {num(), num()} | [{num(), num()}] when
      X :: num() | [num()].
%% @doc Breaks a number into integer and fractional parts

modf(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:modf(X) end, Xs);
modf(X)                   ->
    bmath_libc:modf(X).

-spec next_after(From, To) -> num() when
      From :: num(),
      To :: num().
%% @doc Determines next representable floating-point value towards the given 
%% value 

next_after(From, To) ->
    bmath_libc:next_after(From, To).

-spec copy_sign(X, Y) -> num() when
      X :: num(),
      Y :: num().
%% @doc Produces a value with the magnitude of a given value and the sign of
%% another given value 

copy_sign(X, Y) ->
    bmath_libc:copy_sign(X, Y).

-spec classify(X) -> inf | nan | normal | subnormal | zero | unknown when
      X :: num().
%% @doc Classifies the given floating-point value 

classify(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:classify(X) end, Xs);
classify(X) ->
    bmath_libc:classify(X).

-spec is_normal(X) -> boolean() | [boolean()] when 
      X :: num() | [num()].
%% @doc Checks if the given number is normal.

is_normal(Xs) when is_list(Xs) ->
    lists:map(fun(X) -> bmath_libc:is_normal(X) end, Xs);
is_normal(X) ->
    bmath_libc:is_normal(X).

-spec is_gt(X, Y) -> boolean() when
      X :: num(),
      Y :: num().
%% @doc Checks if the first floating-point argument is greater than the second 

is_gt(X, Y) ->
    bmath_libc:is_gt(X, Y).

-spec is_ge(X, Y) -> boolean() when
      X :: num(),
      Y :: num().
%% @doc Checks if the first floating-point argument is greater or equal than
%% the second 

is_ge(X, Y) ->
    bmath_libc:is_ge(X, Y).

-spec is_lt(_X, _Y) -> boolean() when
      _X :: num(),
      _Y :: num().
%% @doc Checks if the first floating-point argument is less than the second 

is_lt(X, Y) ->
    bmath_libc:is_lt(X, Y).

-spec is_le(X, Y) -> boolean() when
      X :: num(),
      Y :: num().
%% @doc Checks if the first floating-point argument is less or equal than 
%% the second 

is_le(X, Y) ->
    bmath_libc:is_le(X, Y).

-spec is_ltgt(X, Y) -> boolean() when
      X :: num(),
      Y :: num().
%% @doc Checks if the first floating-point argument is less or greater than 
%% the second 

is_ltgt(X, Y) ->
    bmath_libc:is_ltgt(X, Y).

-spec is_unordered(X, Y) -> boolean() when
      X :: num(),
      Y :: num().
%% @doc Checks if two floating-point values are unordered

is_unordered(X, Y) ->
    bmath_libc:is_unordered(X, Y).

%% @doc Checks if the given value is finite
is_finite(X) when is_number(X)        -> true;
is_finite(X) when ?BMATH_NOTFINITE(X) -> false;
is_finite(Xs) when is_list(Xs)        ->
    lists:map(fun(X) -> is_finite(X) end, Xs).

%% @doc Checks if the given value is infinite
is_infinity(X) when ?BMATH_ISINF(X)               -> true;
is_infinity(X) when is_number(X); ?BMATH_ISNAN(X) -> false;
is_infinity(Xs) when is_list(Xs)                  ->
    lists:map(fun(X) -> is_infinity(X) end, Xs).

%% @doc Checks if the given value is NaN 
is_nan(X) when ?BMATH_ISNAN(X)               -> true;
is_nan(X) when is_number(X); ?BMATH_ISINF(X) -> false;
is_nan(Xs) when is_list(Xs)                  ->
    lists:map(fun(X) -> is_nan(X) end, Xs).

-spec fuzzy_compare(X, Y) -> boolean() when
      X :: num(),
      Y :: num().
%% @doc Compares two floats in a relative way to see if they are equal. 

fuzzy_compare(X, Y) ->
    is_le(fmul(fabs(fsub(X,Y)),1000000000000.0),fmin(fabs(X),fabs(Y))).

-spec fuzzy_zero(X) -> boolean() when
      X :: num().
%% @doc Checks if a number is condisered zero. 

fuzzy_zero(X) ->
    is_le(fabs(X), 0.000000000001).

%% @doc Converts degrees to radians
degrees_to_radians(X) -> 
    fmul(X, ?BMATH_PI/180).

%% @doc Converts radians to degrees
radians_to_degrees(X) -> 
    fmul(X, 180/?BMATH_PI).

%% @doc Converts nans and infinities to zero
nan_to_num(X) when is_number(X)        -> X;
nan_to_num(X) when ?BMATH_NOTFINITE(X) -> 0;
nan_to_num(Xs)                         ->
    lists:map(fun(X) -> nan_to_num(X) end, Xs).

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
                        {Len + 1, Sum + pow(X - Mean, 2)};
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

%% @doc Adds a constant value to each element in the list
addc(C, Xs) ->
    lists:map(fun(X) -> fadd(X, C) end, Xs).  

%% @doc Subtracts a constant value from each element in the list
subc(C, Xs) ->
    lists:map(fun(X) -> fsub(X, C) end, Xs).

%% @doc Multiplies each element of a list by a constant
mulc(C, Xs) ->
    lists:map(fun(X) -> fmul(X, C) end, Xs).  

%% @doc Divides each element of a list by a constant
divc(C, Xs) ->
    lists:map(fun(X) -> fmul(X, C) end, Xs).

