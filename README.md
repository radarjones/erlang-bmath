erlang-bmath
=====

Better math library. Wraps many of the C99 library math functions. Erlang lacks many of the standard C library floating point math functions. Its native floating pointer numbers do not support nan or infinity. In bmath when a C math function returns either they are represented as the following atoms.

| Atom   | Description       |
| ------ | ----------------- |
| 'nan'  | positive nan      |
| '-nan' | negative nan      |
| 'inf'  | positive infinity |
| '-inf' | negative infinity |


The driving factor for developing this NIF is my need for floating point remainder, nan and infinities. 

Build
-----

    $ rebar3 compile

Documentation
----
    $ rebar3 edoc

