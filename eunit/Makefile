compile:
	erlc -o ebin/ src/*.erl test/*.erl

eunit:
	erl -sasl errlog_type error -pa ebin/ -eval 'eunit:test(engine,[verbose]), eunit:test(engine_op,[verbose]), eunit:test(engine_proc,[verbose]), init:stop().'
	erl -sasl errlog_type error -pa ebin/ -eval 'eunit:test(temp,[verbose]), init:stop().'


clean:
	rm -rf ebin/*
	rm -rf priv/*
	rm -rf *~
	rm -rf src/*~
	rm -rf src/*.beam
	rm -rf test/*~
	rm -rf test/*.beam

all: clean compile eunit