test: bogo.pl test.c
	swipl-ld -goal true -o test test.c bogo.pl

clean:
	rm -rf test
