BUILD = dune build
EXEC = dune exec
TEST = dune runtest

all:
	$(BUILD)

watch:
	$(BUILD) --watch

run:
	$(BUILD) main.exe
	$(EXEC) ./main.exe

runtests:
	$(TEST)