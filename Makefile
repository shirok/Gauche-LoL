all:

clean:
	rm -f core *~

check:
	gosh ./test.scm

