PREFIX=/usr

EJABBERD_PATH=../ejabberd/
EJABBERD_SRC_PATH=$(EJABBERD_PATH)src

#BEH_BEAMS=gen_hello_world_api.beam

INCLUDE=-I ./include -I $(PREFIX)/lib -pa $(PREFIX)/lib/ejabberd/ebin

BEAMS=mod_hello_world.beam 

ALL_BEAMS=$(BEH_BEAMS) $(BEAMS)

DIST_BEAMS=$(addprefix ebin/,$(ALL_BEAMS))


TEST_BEAMS=test_ebin/tests.beam 

TEST_DIST=$(addprefix test_ebin/,$(ALL_BEAMS)) \

all: $(DIST_BEAMS)

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc -pa ./ebin $(INCLUDE)  -o ./ebin $<

$(TEST_BEAMS): test_ebin/%.beam : tests/%.erl
	@mkdir -p test_ebin
	erlc -Dtest $(INCLUDE) -o ./test_ebin $<

$(TEST_DIST): test_ebin/%.beam : src/%.erl
	@mkdir -p test_ebin
	erlc -Dtest -pa ./test_ebin $(INCLUDE) -o ./test_ebin $<

install: all
	cp ebin/*.beam $(PREFIX)/lib/ejabberd/ebin

build_tests: $(TEST_DIST) $(TEST_BEAMS)

test: build_tests
	@erl \
	    -noshell \
	    -pa ./lib/meck/ebin \
	    -pa ./test_ebin -pa ./include \
	    -eval "tests:all_test()" \
		-s init stop


clean:
	rm -f ebin/*.beam test_ebin/*.beam
