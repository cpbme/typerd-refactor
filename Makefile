COMPILE_PREQ_ARGS	:= --features serde
CARGO				:= cargo

install:
	$(CARGO) install --path . --offline $(COMPILE_PREQ_ARGS)

compiler:
	$(CARGO) build $(COMPILE_PREQ_ARGS)

test-parser:
	$(CARGO) test parser $(COMPILE_PREQ_ARGS)

test:
	$(CARGO) test $(COMPILE_PREQ_ARGS)
