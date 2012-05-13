
-module(temp).

-export([f2c/1, c2f/1]).

-spec f2c(number()) -> float().
f2c(F) -> 5 * (F - 32) / 9.

-spec c2f(number()) -> float().
c2f(C) -> 9 * C / 5 + 32.
