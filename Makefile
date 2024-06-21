BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable
ALL_BUILD_MODES = dev prod

LIB_PROJECT = gnat/gnatformat.gpr

BIN_PROJECT = gnat/gnatformat_driver.gpr

TEST_PROGRAMS = testsuite/test_programs/partial_gnatformat.gpr

.PHONY: all
all: lib bin test-programs

.PHONY: lib
lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprbuild \
			-v \
			-k \
			-XGNATFORMAT_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
			-P $(LIB_PROJECT) \
			-p \
			-j$(PROCESSORS) ; \
	done;

.PHONY: bin
bin:
	gprbuild \
		-v \
		-k \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
		-P$(BIN_PROJECT) \
		-p \
		-j$(PROCESSORS);

.PHONY: clean
clean:
	rm -rf bin;
	rm -rf lib;
	rm -rf obj;

.PHONY: install
install: install-lib install-bin

.PHONY: install-lib
install-lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprinstall \
			-XGNATFORMAT_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
			--install-name=gnatformat \
			--prefix="$(PREFIX)" \
			--sources-subdir=include/lal-refactor \
			--build-name=$$library_type \
			--build-var=LIBRARY_TYPE \
			-P $(LIB_PROJECT) -p -f ; \
	done ;

.PHONY: install-bin
install-bin:
	gprinstall \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE) \
		--install-name=gnatformat_driver \
		--prefix="$(PREFIX)" \
		-P $(BIN_PROJECT) \
		-p \
		-f ;

.PHONY: test-programs
test-programs:
	for proj in $(TEST_PROGRAMS) ; do \
		gprbuild \
			-v \
			-k \
			-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
			-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
			-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
			-P $$proj \
			-p \
			-j$(PROCESSORS) ; \
	done;

.PHONY: install-test-programs
install-test-programs:
	for proj in $(TEST_PROGRAMS) ; do \
		gprinstall \
			-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
			-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
			-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
			--prefix="$(PREFIX)" \
			--install-name=test_programs \
			--mode=usage \
			-P $$proj -p -f ; \
	done ;

.PHONY: test
test:
	python testsuite/testsuite.py
