ROOT_DIR=$(shell pwd)

BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable

LIB_PROJECT = gnat/gnatformat.gpr

BIN_PROJECT = gnat/gnatformat_driver.gpr

TEST_PROGRAMS = testsuite/test_programs/partial_gnatformat.gpr

COVERAGE ?=
COVERAGE_BUILD_FLAGS = --implicit-with=gnatcov_rts \
	--src-subdirs=gnatcov-instr \
	-gnatyN

.PHONY: coverage-setup
coverage-setup:
ifneq ($(COVERAGE),)
	gnatcov setup --prefix gnatcov_rts_prefix
endif

.PHONY: coverage-instrumentation
coverage-instrumentation:
ifneq ($(COVERAGE),)
	rm -rf obj/*gnatcov-instr
	rm -rf obj/*/*gnatcov-instr
	gnatcov \
		instrument \
		--level=stmt+decision \
		--dump-trigger=atexit \
		--no-subprojects \
		-P$(BIN_PROJECT) \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
		--projects=gnatformat \
		--projects=gnatformat_driver;
endif

.PHONY: coverage-run
coverage-run: bin test-programs
ifneq ($(COVERAGE),)
	python testsuite/testsuite.py --gnatcov lib/$(LIBRARY_TYPE).$(BUILD_MODE) obj/$(LIBRARY_TYPE).$(BUILD_MODE) --gnatcov-source-root $(ROOT_DIR)
endif

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
bin: coverage-instrumentation
ifeq ($(COVERAGE),)
	gprbuild \
		-v \
		-k \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
		-P$(BIN_PROJECT) \
		-p \
		-j$(PROCESSORS);
else
	gprbuild \
		-v \
		-k \
		-XGNATFORMAT_LIBRARY_TYPE=static \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
		-P$(BIN_PROJECT) \
		-p \
		$(COVERAGE_BUILD_FLAGS) \
		-j$(PROCESSORS);
endif

.PHONY: clean
clean:
	rm -rf bin;
	rm -rf lib;
	rm -rf obj;
	rm -rf testsuite/test_programs/obj;
	rm -rf testsuite/test_programs/bin;

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
ifneq ($(COVERAGE),)
	mkdir -p $(PREFIX)/share/gnatformat/sids || true
	cp obj/*.sid $(PREFIX)/share/gnatformat/sids/
	cp obj/*/*.sid $(PREFIX)/share/gnatformat/sids/
endif

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
