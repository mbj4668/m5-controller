DESCRIPTION = M5 Controller

DEPS = mjson
dep_mjson = git https://github.com/mbj4668/mjson

ERLC_OPTS =

include erl.mk

erl.mk:
	curl -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@


BEAMS = $(ERL_MODULES:%=ebin/%.beam) ebin/mjson_decode.beam

all: main.avm

main.avm: $(BEAMS)
	../AtomVM/build/tools/packbeam/PackBEAM $@ $^

# AtomVM on ESP32 doesn't have list_to_integer/2, so recompile mjson
ebin/%.beam: deps/mjson/src/%.erl
	erlc -DNO_LIST_TO_INTEGER_2 -DNO_RE_RUN_2 -o ebin $<

clean: my-clean

my-clean:
	rm -f main.avm

flash-and-monitor:
	$(MAKE) flash
	$(MAKE) monitor

flash: main.avm
	sh ../AtomVM/tools/dev/flash.sh -p /dev/ttyACM0 $<

monitor:
	minicom -D /dev/ttyACM0
