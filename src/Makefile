.PHONY: all clean test

all:
	dune build

clean:
	dune clean

test: all
	JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test

test1: all
	JOUJOU="../src/joujou.exe" $(MAKE) -C ../test test1