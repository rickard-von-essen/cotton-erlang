EBIN_DIR=ebin
SOURCE_DIR=src
INCLUDE_DIR=include
ERLC_FLAGS=-W0

compile:
	mkdir -p $(EBIN_DIR)
	erlc +debug_info -I $(INCLUDE_DIR) -o $(EBIN_DIR) $(ERLC_FLAGS) $(SOURCE_DIR)/*.erl

all: compile

test: compile
	erl -pa ebin -noshell -eval 'hessian_test:test(),type_mapping_test:test(),halt().'

test_coverage: compile
	erl -pa ebin -noshell -eval 'hessian_test:test_coverage(),halt().'

clean:
	rm $(EBIN_DIR)/*.beam
	rm -rf cover
	rm -rf cotton
	rm cotton*.tar.gz
	rm erl_crash.*

dist:
	mkdir -p cotton
	cp -r Makefile README include java src cotton/ 
	tar cfvz cotton.tar.gz cotton/
