-module(disp_logger_handler).
-behaviour(gen_event).
 
-export([init/1, 
         handle_event/2, 
         handle_call/2, 
         handle_info/2, 
         code_change/3,
         terminate/2]).
 
init([]) ->
    {ok, []}.

%% Generated when error_msg/1,2 or format is called.
handle_event({error, Gleader, {Pid, Format, Data}}, State) ->
    io:format("Write to logs:~n~p~n",[{error, Gleader, {Pid, Format, Data}}]),
    {ok, State};

%% Generated when error_report/1 is called.
handle_event({error_report, Gleader, {Pid, std_error, Report}}, State) ->
    io:format("Write to logs:~n~p~n",[{error_report, Gleader, {Pid, std_error, Report}}]),
    {ok, State};

%% Generated when error_report/2 is called.    
handle_event({error_report, Gleader, {Pid, Type, Report}}, State) ->
    io:format("Write to logs:~n~p~n",[{error_report, Gleader, {Pid, Type, Report}}]),
    {ok, State};

%% Generated when warning_msg/1,2 is called, provided that warnings are set to be tagged as warnings.  
handle_event({warning_msg, Gleader, {Pid, Format, Data}}, State) ->
    io:format("Write to logs:~n~p~n",[{warning_msg, Gleader, {Pid, Format, Data}}]),
    {ok, State};

%% Generated when warning_report/1 is called, provided that warnings are set to be tagged as warnings.
handle_event({warning_report, Gleader, {Pid, std_warning, Report}}, State) ->
    io:format("Write to logs:~n~p~n",[{warning_report, Gleader, {Pid, std_warning, Report}}]),
    {ok, State};

%% Generated when warning_report/2 is called, provided that warnings are set to be tagged as warnings.
handle_event({warning_report, Gleader, {Pid, Type, Report}}, State) ->
    io:format("Write to logs:~n~p~n",[{warning_report, Gleader, {Pid, Type, Report}}]),
    {ok, State};

%% Generated when info_msg/1,2 is called.
handle_event({info_msg, Gleader, {Pid, Format, Data}}, State) ->
    io:format("Write to logs:~n~p~n",[{info_msg, Gleader, {Pid, Format, Data}}]),
    {ok, State};

%% Generated when info_report/1 is called.
handle_event({info_report, Gleader, {Pid, std_info, Report}}, State) ->
    io:format("Write to logs:~n~p~n",[{info_report, Gleader, {Pid, std_info, Report}}]),
    {ok, State};

%% Generated when info_report/2 is called.
handle_event({info_report, Gleader, {Pid, Type, Report}}, State) ->
    io:format("Write to logs:~n~p~n",[{info_report, Gleader, {Pid, Type, Report}}]),
    {ok, State};

%% Any other unpredicted events are handled here.
handle_event(Sth, State) ->
    io:format("Write to logs (unpredicted event):~n~p~n",[Sth]),
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
        
