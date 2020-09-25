

build:
	mkdir -p ebin
	erlc -o ebin src/type_mapper.erl
	cp -f src/type_mapper.app.src ebin/type_mapper.app

test: build
	mkdir -p log
	ct_run -pa ebin -logdir log

