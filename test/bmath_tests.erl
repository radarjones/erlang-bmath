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

-module(bmath_tests).

-include_lib("eunit/include/eunit.hrl").

fadd_test() ->
    ?assert(bmath:fadd(1, 2) == 3.0),
    ?assert(bmath:fadd(1.5, 2) == 3.5),
    ?assert(bmath:fadd(1, -1) == 0.0),
    ?assert(bmath:fadd(nan, 1) == nan),
    ?assert(bmath:fadd('-nan', 1) == '-nan'),
    ?assert(bmath:fadd(inf, 1) == inf),
    ?assert(bmath:fadd('-inf', 1) == '-inf').

fsub_test() ->
    ?assert(bmath:fsub(2, 1) == 1.0),
    ?assert(bmath:fsub(1.5, 2) == -0.5),
    ?assert(bmath:fsub(1, -1) == 2.0),
    ?assert(bmath:fsub(nan, 1) == nan),
    ?assert(bmath:fsub('-nan', 1) == '-nan'),
    ?assert(bmath:fsub(inf, 1) == inf),
    ?assert(bmath:fsub('-inf', 1) == '-inf').

fmul_test() ->
    ?assert(bmath:fmul(2, 3) == 6.0),
    ?assert(bmath:fmul(1.5, 2.5) == 3.75),
    ?assert(bmath:fmul(-2, 3) == -6.0),
    ?assert(bmath:fmul(nan, 1) == nan),
    ?assert(bmath:fmul('-nan', 1) == '-nan'),
    ?assert(bmath:fmul(inf, 1) == inf),
    ?assert(bmath:fmul('-inf', 1) == '-inf'),
    ?assert(bmath:fmul(1.0e200, 1.0e200) == inf),
    ?assert(bmath:fmul(-1.0e200, 1.0e200) == '-inf').

fdiv_test() ->
    ?assert(bmath:fdiv(1, 2) == 0.5),
    ?assert(bmath:fdiv(-1, 2) == -0.5),
    ?assert(bmath:fdiv(1.0, 0.0) == inf),
    ?assert(bmath:fdiv(nan, 1) == nan),
    ?assert(bmath:fdiv('-nan', 1) == '-nan'),
    ?assert(bmath:fdiv(inf, 1) == inf),
    ?assert(bmath:fdiv('-inf', 1) == '-inf').

fabs_test() ->
    ?assert(bmath:fabs(-1.0) == 1.0),
    ?assert(bmath:fabs(-1) == 1.0),
    ?assert(bmath:fabs(1.0) == 1.0),
    ?assert(bmath:fabs(1) == 1.0).

fmod_test() ->
    ?assert(bmath:fmod(5.5, 5) == 0.5),
    ?assert(bmath:fmod(-5.5, 5) == -0.5),
    ?assert(bmath:fmod(0, 1) == 0.0),
    ?assert(bmath:fmod(5.1, inf) == 5.1),
    ?assert(bmath:fmod(5.1, 0) == '-nan').

remainder_test() ->
    ?assert(bmath:remainder(5.5, 5) == 0.5),
    ?assert(bmath:remainder(-5.5, 5) == -0.5),
    ?assert(bmath:remainder(0, 1) == 0.0).

remquo_test() ->
    ?assertMatch({'-nan', 1}, bmath:remquo(inf, 1)).

stddev_test() ->
    ?assert(bmath:stddev([]) == 0),
    ?assert(bmath:stddev([2,4,4,4,5,5,7,9]) == 2),
    ?assert(bmath:stddev([inf,'-inf',nan,'-nan',2,4,4,4,5,5,7,9]) == 2).

