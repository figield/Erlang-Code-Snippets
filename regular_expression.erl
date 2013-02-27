-module(regular_expression).

-compile(export_all).

validate_fingerprint() ->
    FP1 = "METHOD0 99:38:FB:D7:BF:B8:93:DE:1F:9C:7F:63:AA:3C:D0:2B:B1:0E:1D:5D ",      %% Wrong alg
    FP2 = "METHOD1    99:38:FB:D7:BF:B8:93:DE:1F:9C:7F:63:AA:3C:D0:2B:B1:0E:1D ",      %% too short key
    FP3 = "METHOD1  99:38:FB:D7:BF:B8:93:DE:1F:9C:7F:63:AA:3C:D0:2B:B1:0E:1D:5D:5D",   %% too long key
    FP4 = "METHOD1  99:38:FB:D7:BF:B8:93:DE:1F:9C:7F:63:AA:3C:D0:2B:B1:0E:1D5D ",      %% missing colon
    FP5 = "METHOD1 99:38:FB:D7:BF:B8:93:DE:1F:9C:7F:63:AA:3C:D0:2B:B1:0E:1D:5D ",      %% correct

    lists:foreach(fun(Fingerprint) -> 
                          R = validate_fingerprint(Fingerprint),
                          io:format("~p:~n~p~n",[Fingerprint, R])                          
                  end,[FP1,FP2,FP3,FP4,FP5]).

validate_fingerprint(Fingerprint)->
    FingerprintLower = string:to_lower(Fingerprint),
    case string:tokens(FingerprintLower, " ") of
        ["method1" | Key]  ->
            case validate_key(Key) of
                ok ->
                    {ok, Fingerprint};
                _ ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.
    
validate_key(Key)->
    case re:run(Key, "^(([a-f0-9]{2}:){19}([a-f0-9]{2}))$",[{capture,[1]}]) of
        {match, [{0,59}]} ->
            ok;
        _ ->
            invalid
    end.
