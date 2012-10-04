
CFLAGS=-Wall

spektro: spektro.c
	gcc -O9 $(CFLAGS) -o spektro spektro.c && cp spektro ../bin

spektro.c: spektro.scm ../bin/owl
	../bin/owl -c spektro.scm 

../bin/owl:
	curl http://owl-lisp.googlecode.com/files/owl-0.1-r45.c.gz | gzip -d | gcc -O2 -x c -o ../bin/owl -


