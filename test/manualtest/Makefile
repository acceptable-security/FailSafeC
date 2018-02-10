all:
	@echo "$(MAKE): see Makefile and source files for details (and analyze it)."
	@false

TARGETS = environ mmap_test1 fib \
	setjmp_test1 setjmp_test2 setjmp_test3 \
	sigsetjmp_test1 sigsetjmp_test2 sigsetjmp_test3 \
	test_env_exec

SAFE_TARGETS = $(TARGETS)

NATIVE_TARGETS = $(TARGETS)

BUILD_ALL_TARGETS = $(addsuffix .safe,$(TARGETS)) $(addsuffix .native,$(TARGETS))

build: $(BUILD_ALL_TARGETS) subdir

subdir: bss-test

.PHONY: bss-test
bss-test:
	make -C $@ all

mmap_test1_libs = -lmmap_slow

%.safe: %.c
	../../driver/fscc $($@_opts) -o $@ $^ $($*_libs)

%.native: %.c
	gcc $($@_native_opts) -o $@ $^ $($@_native_libs)

clean:
	make -C bss-test clean
	rm -f a.out *..linkstub.* *.safe.[co]
	rm -f $(BUILD_ALL_TARGETS)