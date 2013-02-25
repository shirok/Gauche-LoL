all:

clean:
	rm -f core *~ *.dot *.dot.png

check:
	gosh ./test.scm

