test: bogo.pl test.c
	swipl-ld -goal true -o test test.c bogo.pl
	./test

clean:
	rm -rf test
