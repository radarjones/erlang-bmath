/* vim: set ts=8 sts=8 sw=8 tw=79 noet :*/
/*
 * Copyright (c) 2016, Mark Jones <markalanj@gmail.com>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * The names of its contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <erl_nif.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#define ATOM_POS_NAN	"nan"
#define ATOM_NEG_NAN	"-nan"
#define ATOM_POS_INF	"inf"
#define ATOM_NEG_INF	"-inf"

struct bmath_priv {
	unsigned int vsn;
	ERL_NIF_TERM atom_true;
	ERL_NIF_TERM atom_false;
	ERL_NIF_TERM atom_pos_nan;
	ERL_NIF_TERM atom_neg_nan;
	ERL_NIF_TERM atom_pos_inf;
	ERL_NIF_TERM atom_neg_inf;
};

static int load_count;

static ERL_NIF_TERM
make_atom(ErlNifEnv *env, const char *str)
{
	ERL_NIF_TERM atom;

	if(!enif_make_existing_atom(env, str, &atom, ERL_NIF_LATIN1))
		atom = enif_make_atom(env, str);

	return atom;
}

static ERL_NIF_TERM
fpclassify_to_atom(ErlNifEnv *env, int arg)
{
	ERL_NIF_TERM ret;
	struct bmath_priv *priv = enif_priv_data(env);

	switch(arg) {
	case FP_INFINITE:
		ret = priv->atom_pos_inf;
		break;
	case FP_NAN:
		ret = priv->atom_pos_nan;
		break;
	case FP_NORMAL:
		ret = make_atom(env, "normal");
		break;
	case FP_SUBNORMAL:
		ret = make_atom(env, "subnormal");
		break;
	case FP_ZERO:
		ret = make_atom(env, "zero");
		break;
	default:
		ret = make_atom(env, "unknown");
		break;
	}
	return ret;
}

static bool
get_double(ErlNifEnv *env, ERL_NIF_TERM term, double *x)
{
	unsigned int sz;
	ErlNifSInt64 i;
	struct bmath_priv *priv = enif_priv_data(env);

	if(enif_get_double(env, term, x))
		return true;

	if(enif_get_int64(env, term, &i)) {
		*x = i;
		return true;
	}

	/* Calling enif_compare at a minimum wreaks remquo results. Calling
	 * enif_is_identical results in worse behavior. Comparing atoms as 
	 * strings seems to be ok */

#undef WREAK_REMQUO
#ifdef WREAK_REMQUO
	enif_compare(term, priv->atom_pos_nan);
#endif

#if 1
	if(enif_get_atom_length(env, term, &sz, ERL_NIF_LATIN1)) {
		char buf[sz + 1];
		enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1);
		if(strcmp(buf, ATOM_POS_NAN) == 0) {
			*x = NAN;
			return true;
		}
		if(strcmp(buf, ATOM_NEG_NAN) == 0) {
			*x = -NAN;
			return true;
		}
		if(strcmp(buf, ATOM_POS_INF) == 0) {
			*x = INFINITY;
			return true;
		}
		if(strcmp(buf, ATOM_NEG_INF) == 0) {
			*x = -INFINITY;
			return true;
		}
	}
#else
	if(enif_is_atom(env, term)) {
		if(enif_compare(term, priv->atom_pos_nan) == 0) {
			*x = NAN;
			printf("nan\r\n");
			return true;
		}
		if(enif_compare(term, priv->atom_neg_nan) == 0) {
			*x = -NAN;
			printf("-nan\r\n");
			return true;
		}
		if(enif_compare(term, priv->atom_pos_inf) == 0) {
			*x = INFINITY;
			printf("inf\r\n");
			return true;
		}
		if(enif_compare(term, priv->atom_neg_inf) == 0) {
			*x = -INFINITY;
			printf("-inf\r\n");
			return true;
		}
	}
#endif
	return false;
}

static ERL_NIF_TERM
nan_to_atom(ErlNifEnv *env, double x)
{
	struct bmath_priv *priv = enif_priv_data(env);

	if(signbit(x))
		return priv->atom_neg_nan;
	else
		return priv->atom_pos_nan;
}

static ERL_NIF_TERM
inf_to_atom(ErlNifEnv *env, double x)
{
	struct bmath_priv *priv = enif_priv_data(env);

	if(signbit(x))
		return priv->atom_neg_inf;
	else
		return priv->atom_pos_inf;
}

static ERL_NIF_TERM
make_math_result(ErlNifEnv *env, double x)
{
	switch(fpclassify(x)) {
	case FP_INFINITE:
		return inf_to_atom(env, x);
	case FP_NAN:
		return nan_to_atom(env, x);
	default:
		return enif_make_double(env, x);
	}
}

static ERL_NIF_TERM
nif_fadd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, x + y);
}

static ERL_NIF_TERM
nif_fsub(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, x - y);
}

static ERL_NIF_TERM
nif_fmul(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, x * y);
}

static ERL_NIF_TERM
nif_fdiv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, x / y);
}

static ERL_NIF_TERM
nif_fabs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, fabs(x));
}

static ERL_NIF_TERM
nif_fmod(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, fmod(x, y));
}

static ERL_NIF_TERM
nif_remainder(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, remainder(x, y));
}

static ERL_NIF_TERM
nif_remquo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y, rem;
	int quo;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	rem = remquo(x, y, &quo);
#if 0
	printf("remquo(%f, %f) = {%f, %d}\r\n", x, y, rem, quo);
#endif
	return enif_make_tuple2(env,
	                        make_math_result(env, rem),
	                        enif_make_int64(env, quo));
}

static ERL_NIF_TERM
nif_fma(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y, z;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(!get_double(env, argv[2], &z))
		return enif_make_badarg(env);

	return make_math_result(env, fma(x, y, z));
}

static ERL_NIF_TERM
nif_fmax(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, fmax(x, y));
}

static ERL_NIF_TERM
nif_fmin(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, fmin(x, y));
}

static ERL_NIF_TERM
nif_fdim(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, fdim(x, y));
}

static ERL_NIF_TERM
nif_exp(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, exp(x));
}

static ERL_NIF_TERM
nif_exp2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, exp2(x));
}

static ERL_NIF_TERM
nif_expm1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, expm1(x));
}

static ERL_NIF_TERM
nif_log(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, log(x));
}

static ERL_NIF_TERM
nif_log10(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, log10(x));
}

static ERL_NIF_TERM
nif_log2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, log2(x));
}

static ERL_NIF_TERM
nif_log1p(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, log1p(x));
}

static ERL_NIF_TERM
nif_pow(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double base, exp;

	if(!get_double(env, argv[0], &base))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &exp))
		return enif_make_badarg(env);

	return make_math_result(env, pow(base, exp));
}

static ERL_NIF_TERM
nif_sqrt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, sqrt(x));
}

static ERL_NIF_TERM
nif_cbrt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, cbrt(x));
}

static ERL_NIF_TERM
nif_hypot(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, hypot(x, y));
}

static ERL_NIF_TERM
nif_sin(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, sin(x));
}

static ERL_NIF_TERM
nif_cos(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, cos(x));
}

static ERL_NIF_TERM
nif_tan(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, tan(x));
}

static ERL_NIF_TERM
nif_asin(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, asin(x));
}

static ERL_NIF_TERM
nif_acos(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, acos(x));
}

static ERL_NIF_TERM
nif_atan(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, atan(x));
}

static ERL_NIF_TERM
nif_atan2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double y, x;

	if(!get_double(env, argv[0], &y))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &x))
		return enif_make_badarg(env);

	return make_math_result(env, atan2(y, x));
}

static ERL_NIF_TERM
nif_sinh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, sinh(x));
}

static ERL_NIF_TERM
nif_cosh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, cosh(x));
}

static ERL_NIF_TERM
nif_tanh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, tanh(x));
}

static ERL_NIF_TERM
nif_asinh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, asinh(x));
}

static ERL_NIF_TERM
nif_acosh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, acosh(x));
}

static ERL_NIF_TERM
nif_atanh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, atanh(x));
}

static ERL_NIF_TERM
nif_erf(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, erf(x));
}

static ERL_NIF_TERM
nif_erfc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, erfc(x));
}

static ERL_NIF_TERM
nif_tgamma(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, tgamma(x));
}

static ERL_NIF_TERM
nif_lgamma(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, lgamma(x));
}

static ERL_NIF_TERM
nif_ceil(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, ceil(x));
}

static ERL_NIF_TERM
nif_floor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, floor(x));
}

static ERL_NIF_TERM
nif_trunc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, trunc(x));
}

static ERL_NIF_TERM
nif_round(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return make_math_result(env, round(x));
}

static ERL_NIF_TERM
nif_modf(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, fpart, ipart;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	fpart = modf(x, &ipart);
	return enif_make_tuple2(env,
	                        make_math_result(env, ipart),
	                        make_math_result(env, fpart));
}

static ERL_NIF_TERM
nif_nextafter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double from, to;

	if(!get_double(env, argv[0], &from))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &to))
		return enif_make_badarg(env);

	return make_math_result(env, nextafter(from, to));
}

static ERL_NIF_TERM
nif_copysign(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double y, x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	return make_math_result(env, copysign(x, y));
}

static ERL_NIF_TERM
nif_fpclassify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	return fpclassify_to_atom(env, fpclassify(x));
}

static ERL_NIF_TERM
nif_isnormal(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(isnormal(x))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ERL_NIF_TERM
nif_isgreater(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(isgreater(x, y))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ERL_NIF_TERM
nif_isgreaterequal(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(isgreaterequal(x, y))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ERL_NIF_TERM
nif_isless(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(isless(x, y))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ERL_NIF_TERM
nif_islessequal(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(islessequal(x, y))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ERL_NIF_TERM
nif_islessgreater(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(islessgreater(x, y))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ERL_NIF_TERM
nif_isunordered(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	double x, y;
	struct bmath_priv *priv = enif_priv_data(env);

	if(!get_double(env, argv[0], &x))
		return enif_make_badarg(env);

	if(!get_double(env, argv[1], &y))
		return enif_make_badarg(env);

	if(isunordered(x, y))
		return priv->atom_true;
	else
		return priv->atom_false;
}

static ErlNifFunc nif_funcs[] = {
	// basic operations
	{"fadd",		2, nif_fadd},
	{"fsub",		2, nif_fsub},
	{"fmul",		2, nif_fmul},
	{"fdiv",		2, nif_fdiv},
	{"fabs",		1, nif_fabs},
	{"fmod",		2, nif_fmod},
	{"remainder",		2, nif_remainder},
	{"remquo",		2, nif_remquo},
	{"fma",			3, nif_fma},
	{"fmax",		2, nif_fmax},
	{"fmin",		2, nif_fmin},
	{"fdim",		2, nif_fdim},
	// exponential functions
	{"exp",			1, nif_exp},
	{"exp2",		1, nif_exp2},
	{"expm1",		1, nif_expm1},
	{"log",			1, nif_log},
	{"log10",		1, nif_log10},
	{"log2",		1, nif_log2},
	{"log1p",		1, nif_log1p},
	// power functions
	{"pow",			2, nif_pow},
	{"sqrt",		1, nif_sqrt},
	{"cbrt",		1, nif_cbrt},
	{"hypot",		2, nif_hypot},
	// trigonometric functions
	{"sin",			1, nif_sin},
	{"cos",			1, nif_cos},
	{"tan",			1, nif_tan},
	{"asin",		1, nif_asin},
	{"acos",		1, nif_acos},
	{"atan",		1, nif_atan},
	{"atan2",		2, nif_atan2},
	// hyperbolic functions
	{"sinh",		1, nif_sinh},
	{"cosh",		1, nif_cosh},
	{"tanh",		1, nif_tanh},
	{"asinh",		1, nif_asinh},
	{"acosh",		1, nif_acosh},
	{"atanh",		1, nif_atanh},
	// error and gamma functions
	{"erf",			1, nif_erf},
	{"erfc",		1, nif_erfc},
	{"tgamma",		1, nif_tgamma},
	{"lgamma",		1, nif_lgamma},
	// nearest integer floating-point operations
	{"ceil",		1, nif_ceil},
	{"floor",		1, nif_floor},
	{"trunc",		1, nif_trunc},
	{"round",		1, nif_round},
	// floating-point manipulation functions
	{"modf",		1, nif_modf},
	{"next_after",		2, nif_nextafter},
	{"copy_sign",		2, nif_copysign},
	// classification and comparison
	{"classify",		1, nif_fpclassify},
	{"is_normal",		1, nif_isnormal},
	{"is_gt",		2, nif_isgreater},
	{"is_ge",		2, nif_isgreaterequal},
	{"is_lt",		2, nif_isless},
	{"is_le",		2, nif_islessequal},
	{"is_ltgt",		2, nif_islessgreater},
	{"is_unordered",	2, nif_isunordered}
};

static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	/* Initialize private data. */
	*priv_data = enif_alloc(sizeof(struct bmath_priv));
	if(*priv_data == NULL)
		return -1;

	struct bmath_priv *priv = *priv_data;
	priv->vsn = 0;
	priv->atom_true = make_atom(env, "true");
	priv->atom_false = make_atom(env, "false");
	priv->atom_pos_nan = make_atom(env, ATOM_POS_NAN);
	priv->atom_neg_nan = make_atom(env, ATOM_NEG_NAN);
	priv->atom_pos_inf = make_atom(env, ATOM_POS_INF);
	priv->atom_neg_inf = make_atom(env, ATOM_NEG_INF);

	load_count++;
	return 0;
}

static int
upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
        ERL_NIF_TERM load_info)
{
	/* Convert private data to new version */
	*priv_data = *old_priv_data;

	load_count++;
	return 0;
}

static void
unload(ErlNifEnv *env, void *priv_data)
{
	if(load_count == 1) {
		/* Destroy the private data. */
		enif_free(priv_data);
	}

	load_count--;
}

ERL_NIF_INIT(bmath, nif_funcs, load, NULL, upgrade, unload)

