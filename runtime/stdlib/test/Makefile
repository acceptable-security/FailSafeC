all:
	@echo "$(MAKE): see Makefile and source files for details (and analyze it)."
	@false

TARGETS = ctype_test errno_test fopen_test fread_test \
	malloc_untyped_test malloc_untyped_test_2 \
	memmove_test mmap_test printf_test \
	realloc_test realloc_test_2 realloc_test_4 realloc_test_8 \
	scanf_test scanf_test_char scanf_test_float scanf_test_n scanf_test_num scanf_test_str \
	signal_test_1 sprintf_test sscanf_test stdarg_test strtests \
	test_stdio test_stdio_2 time_test

SAFE_TARGETS = $(TARGETS) \
	malloc_typed_test malloc_typed_test_2

NATIVE_TARGETS = $(TARGETS)

FSCNATIVE_TARGETS = safebuf_test
# native objects which requires Fail-Safe C runtime

#	safebuf_test \

BUILD_SAFE_TARGETS = $(addsuffix .safe,$(SAFE_TARGETS))
BUILD_NATIVE_TARGETS = $(addsuffix .native,$(NATIVE_TARGETS))
BUILD_OTHER_TARGETS = $(addsuffix .fscnative,$(FSCNATIVE_TARGETS))
BUILD_ALL_TARGETS = $(BUILD_SAFE_TARGETS) $(BUILD_NATIVE_TARGETS) $(BUILD_OTHER_TARGETS)

FSCC=../../../driver/fscc

tests: safe other subdir

safe: $(BUILD_SAFE_TARGETS)

native: $(BUILD_NATIVE_TARGETS)

other: $(BUILD_OTHER_TARGETS)

# stdio-bad, stdio-bad2: should produce compile error in FSC
error-targets: stdio-bad.safe stdio-bad.safe

.PHONY: subdir
subdir: 

mmap_test_libs = -lmmap_slow -lfsc

%.safe: %.c
	$(FSCC) $(FSCC_CFLAGS) $($*_opts) -o $@ $^ $($*_libs)

%.fscnative: %.c
	$(FSCC) $(FSCC_CFLAGS) -ftrust-intermodule-struct -o $@ -xnative-c $($*_opts) $^ $($*_libs)

%.native: %.c
	gcc $($*_native_opts) -o $@ $^ $($*_native_libs)

clean:
	rm -f a.out *..linkstub.* *.safe.[co]
	rm -f $(BUILD_ALL_TARGETS)