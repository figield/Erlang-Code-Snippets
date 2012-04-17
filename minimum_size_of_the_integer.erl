-module(minimum_size_of_the_integer).

-export([get_minimum_size_in_bits/1]).
-export([get_minimum_size_in_bits_by_log2/1]).
-export([run_test/0]).

get_minimum_size_in_bits(Integer) when is_list(Integer) ->
    get_minimum_size_in_bits(list_to_integer(Integer));
get_minimum_size_in_bits(0) ->
    1;
get_minimum_size_in_bits(Integer) when is_integer(Integer) ->
    HowManyBits = ((Integer div 255) + 1) * 8,
    BinaryInteger = <<Integer:HowManyBits>>,
    get_rid_of_zeros_on_the_beginning_and_return_size(BinaryInteger).

get_rid_of_zeros_on_the_beginning_and_return_size(<<>>) ->
    0;
get_rid_of_zeros_on_the_beginning_and_return_size(<<0:1, RestOfBinaryInteger/bitstring>>) ->
    get_rid_of_zeros_on_the_beginning_and_return_size(RestOfBinaryInteger);
get_rid_of_zeros_on_the_beginning_and_return_size(<<1:1, _/bitstring>> = BinaryInteger) -> 
    bit_size(BinaryInteger).



%% SMARTER WAY

%% logA(B) = logC(B)/logC(A).
%% log2(Int) = log10(Int)/log10(2).
get_minimum_size_in_bits_by_log2(0) -> 1;
get_minimum_size_in_bits_by_log2(Integer) when is_integer(Integer) ->
    trunc(math:log10(Integer) / math:log10(2)) + 1.



run_test()->
    true = lists:all(fun(Num)->
                             R1 = get_minimum_size_in_bits(Num),
                             R2 = get_minimum_size_in_bits_by_log2(Num),
                             io:format("~p,",[Num]),
                             R1 == R2
                     end,lists:seq(0,1025)).
