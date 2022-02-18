BUILD = dune build
EXEC = dune exec

all:
	$(BUILD)

watch:
	$(BUILD) --watch

run:
	$(BUILD) main.exe
	$(EXEC) ./main.exe