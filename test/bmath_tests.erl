%%% vim: set ts=4 sts=4 sw=4 tw=79 et :
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

stddev_test() ->
    ?assert(bmath:stddev([2,4,4,4,5,5,7,9]) == 2),
    ?assert(bmath:stddev([inf,'-inf',nan,'-nan',2,4,4,4,5,5,7,9]) == 2).

