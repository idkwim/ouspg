
CFLAGS=-Wall

# new platypus 

test:
	../bin/ol --run platy.scm -- ~/owl-lisp/bench/*.scm

platy: platy.c
	gcc -O3 $(CFLAGS) -o platy platy.c

platy.c: ../bin/ol platy.scm
	../bin/ol --native -c platy.scm
	#../bin/ol -c platy.scm # slow but takes less time for gcc to compile

# old platypus

platypus: platypus.c
	gcc -O3 $(CFLAGS) -o platypus platypus.c && cp platypus ../bin

platypus.c: ../bin/ol platypus.scm
	../bin/ol --native -c platypus.scm
	#../bin/ol -c platypus.scm

../bin/ol:
	curl http://owl-lisp.googlecode.com/svn/trunk/ol.c.gz | gzip -d | gcc -O2 -x c -o ../bin/ol -



