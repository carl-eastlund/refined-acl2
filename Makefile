
MAIN = main.rkt
TEST = tests/run.rkt
EXAMPLES = examples/*.rkt

.PHONY: all
all: build certify examples test

.PHONY: bug
bug:
	raco make kernel.rkt 2> debug.txt
	raco-make-clean examples/bug*
	raco make examples/bug.rkt 2> debug.txt
	racket examples/bug.rkt 2>> debug.txt

.PHONY: build
build:
	raco make $(MAIN)

.PHONY: test
test: build
	raco make $(TEST)
	racket $(TEST)

.PHONY: test-gui
test-gui: build
	raco make $(TEST)
	spawn racket $(TEST) --gui

.PHONY: certify
certify: build
	raco make certify.rkt
	raco certify --verbose $(MAIN) > certify.txt

.PHONY: build-examples
build-examples: build
	raco make $(EXAMPLES)

.PHONY: examples
examples: build-examples certify
	raco certify --verbose $(EXAMPLES) > examples.txt
