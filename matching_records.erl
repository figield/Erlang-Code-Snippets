-module(matching_records).

%% easy example to exaplain some complexities related to records

-export([test/1, test/0]).

-record('The_Record',{field1 = 'val1',
                      field2 = 'val2'}).

test(S)->
    R = #'The_Record'{field1 = 'something',
                      field2 = 'sth'},
    #'The_Record'{field1 = S} = R,
    io:format("S = ~p~n",[S]).

test()->
    R = #'The_Record'{field1 = 'something',
                      field2 = 'sth'},
    #'The_Record'{field1 = S} = R,
    io:format("S = ~p~n",[S]).

