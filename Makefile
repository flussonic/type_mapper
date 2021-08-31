

build:
	mkdir -p ebin
	for s in src/*.erl ; do erlc -o ebin/ $$s ; done
	cp -f src/type_mapper.app.src ebin/type_mapper.app

test: build
	mkdir -p log
	ct_run -pa ebin -logdir log

