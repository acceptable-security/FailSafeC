all: a b c

clean:
	rm -f a b c test2.o test2b.o test2b.a

a: test2.o
	$(CC) -o a test2.o

b: test2.o test2b.o
	$(CC) -o b test2.o test2b.o

c: test2.o test2b.a
	$(CC) -o c test2.o test2b.a

test2.o: test2.c
	$(CC) -c test2.c

test2b.o: test2b.c
	$(CC) -c test2b.c

test2b.a: test2b.o
	rm -f test2b.a
	ar r test2b.a test2b.o


