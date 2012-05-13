compile:
	erlc -o ebin/ src/*.erl test/*.erl

ct:
	ct_run -dir ebin/ -logdir priv/ -suite temp_SUITE.beam -no_auto_compile -pa ebin/ -config cfg/conf.cfg

ct_old:
	run_test -dir ebin/ -logdir priv/ -suite temp_SUITE.beam -no_auto_compile -pa ebin/ -config cfg/conf.cfg

clean:
	rm -rf ebin/*
	rm -rf priv/*
	rm -rf *~
	rm -rf src/*~
	rm -rf src/*.beam
	rm -rf test/*~
	rm -rf test/*.beam

all: clean compile ct
#all: clean compile ct_old

#http://www.erlang.org/doc/apps/common_test/run_test_chapter.html