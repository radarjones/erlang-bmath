%%%============================================================================
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
%%%============================================================================

-define(BMATH_C, 299792458).			% speed of light m/s
-define(BMATH_E, 2.7182818284590452354).	% e
-define(BMATH_EPSILON, 2.2204460492503131e-16).
-define(BMATH_PI, 3.14159265358979323846). 	% pi 
-define(BMATH_PI_2, 1.57079632679489661923). 	% pi/2
-define(BMATH_PI_4, 0.78539816339744830962). 	% pi/4
-define(BMATH_1_PI, 0.31830988618379067154).	% 1/pi
-define(BMATH_2_PI, 0.63661977236758134308).	% 2/pi
-define(BMATH_2SQRTPI, 1.12837916709551257390). % 2/sqrt(pi)
-define(BMATH_SQRT2, 1.41421356237309504880).	% sqrt(2)
-define(BMATH_SQRT1_2, 0.70710678118654752440).	% 1/sqrt(2)
-define(BMATH_GOLDEN_RATIO, 1.61803398875).

-define(BMATH_ISNAN(X), X == nan orelse X == '-nan').
-define(BMATH_ISINF(X), X == inf orelse X == '-inf').
-define(BMATH_NOTFINITE(X), ?BMATH_ISNAN(X) orelse ?BMATH_ISINF(X)). 
