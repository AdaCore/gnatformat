BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable
ALL_BUILD_MODES = dev prod

LIB_PROJECT = gnat/gnatfmt.gpr

BIN_PROJECT = gnat/gnatfmt_driver.gpr

TEST_PROGRAMS = gnat/test_programs.gpr

.PHONY: all
all: lib bin

.PHONY: lib
lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprbuild \
			-v \
			-k \
			-XGNATFMT_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XGNATFMT_BUILD_MODE=$(BUILD_MODE) \
			-P $(LIB_PROJECT) \
			-p \
			-j$(PROCESSORS) ; \
	done;

.PHONY: bin
bin:
	gprbuild \
		-v \
		-k \
		-XGNATFMT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFMT_BUILD_MODE=$(BUILD_MODE) \
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
			-XGNATFMT_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XGNATFMT_BUILD_MODE=$(BUILD_MODE) \
			--install-name=gnatfmt \
			--prefix="$(PREFIX)" \
			--sources-subdir=include/lal-refactor \
			--build-name=$$library_type \
			--build-var=LIBRARY_TYPE \
			-P $(LIB_PROJECT) -p -f ; \
	done ;

.PHONY: install-bin
install-bin:
	gprinstall \
		-XGNATFMT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE) \
		--install-name=gnatfmt_driver \
		--prefix="$(PREFIX)" \
		-P $(BIN_PROJECT) \
		-p \
		-f ;

.PHONY: test-programs
test-programs:
	gprbuild \
		-v \
		-k \
		-XGNATFMT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFMT_BUILD_MODE=$(BUILD_MODE) \
		-P$(TEST_PROGRAMS) \
		-p \
		-j$(PROCESSORS);

.PHONY: install-test-programs
install-test-programs:
	gprinstall \
		-XGNATFMT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE) \
		--install-name=test_programs \
		--prefix="$(PREFIX)" \
		-P $(TEST_PROGRAMS) \
		-p \
		-f ;
