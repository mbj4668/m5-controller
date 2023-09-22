PROJECT = m5-controller
PROJECT_VERSION = 0.1

# AtomVM on ESP32 doesn't have list_to_integer/2
ERLC_OPTS += -DNO_LIST_TO_INTEGER_2

include erlang.mk

BEAMS = ebin/main.beam $(filter-out ebin/main.beam, $(wildcard ebin/*.beam))

main.avm: $(BEAMS)
	../AtomVM/build/tools/packbeam/PackBEAM $@ $^

clean::
	rm -f main.avm

flash-and-montior:
	$(MAKE) flash
	$(MAKE) monitor

flash: main.avm
	sh ../AtomVM/tools/dev/flash.sh -p /dev/ttyACM0 $<

monitor:
	minicom -D /dev/ttyACM0
