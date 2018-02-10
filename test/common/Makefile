all:

test:
	./runtest.sh

clean:
	rm -f *.test.safe.[co] *.test..linkstub.safe.[co] \
	      *.test..linkstub.script \
	      stub.safe.[co] \
	      test..linkstub.safe.[co] test..linkstub.script \
	      *.test..linkstub.safe.[co] *.test..linkstub.script \
	      test driver testlist.c test_stdout test_temp2 \
	      test.log test.err.log
	rm -rf test_temp_dir

.PHONY: test
