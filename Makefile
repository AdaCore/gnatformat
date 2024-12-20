ROOT_DIR=$(shell pwd)

BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable

GNATFORMAT_LIBRARY_PROJECT = gnat/gnatformat.gpr

GNATFORMAT_DRIVER_PROJECT = gnat/gnatformat_driver.gpr

PARTIAL_GNATFORMAT_DRIVER_PROJECT = testsuite/partial_gnatformat/partial_gnatformat.gpr

COVERAGE ?=
COVERAGE_BUILD_FLAGS = \
	--implicit-with=gnatcov_rts \
	--src-subdirs=gnatcov-instr \
	-gnatyN
BASE_BUILD_FLAGS = \
	-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
	-v \
	-k \
	-p \
	-j$(PROCESSORS)
ifeq ($(COVERAGE),)
	COMMON_BUILD_FLAGS = \
		$(BASE_BUILD_FLAGS)
else
	COMMON_BUILD_FLAGS = \
		$(BASE_BUILD_FLAGS) \
		$(COVERAGE_BUILD_FLAGS)
endif
COMMON_INSTRUMENT_FLAGS = \
	--level=stmt+decision \
	--dump-trigger=atexit \
	--no-subprojects \
	-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
	-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
	-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
	--projects=gnatformat

.PHONY: all
all: lib bin partial-gnatformat

.PHONY: lib
lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprbuild \
			-P $(GNATFORMAT_LIBRARY_PROJECT) \
			-XGNATFORMAT_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			$(COMMON_BUILD_FLAGS) ; \
	done ;

.PHONY: bin
bin: coverage-instrumentation
	gprbuild \
		-P $(GNATFORMAT_DRIVER_PROJECT) \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		$(COMMON_BUILD_FLAGS) ;

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
			--sources-subdir=include/gnatformat \
			--build-name=$$library_type \
			--build-var=LIBRARY_TYPE \
			-P $(GNATFORMAT_LIBRARY_PROJECT) \
			-p \
			-f ; \
	done ;

.PHONY: install-bin
install-bin:
	gprinstall \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE) \
		--install-name=gnatformat \
		--prefix="$(PREFIX)" \
		-P $(GNATFORMAT_DRIVER_PROJECT) \
		-p \
		-f ;
ifneq ($(COVERAGE),)
	mkdir -p $(PREFIX)/share/gnatformat/sids || true
	cp obj/*.sid $(PREFIX)/share/gnatformat/sids/
	cp obj/*/*.sid $(PREFIX)/share/gnatformat/sids/
endif

.PHONY: install-bin-stripped
install-bin-stripped:
	gprinstall \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE) \
		--install-name=gnatformat \
		--prefix="$(PREFIX)" \
		-P $(GNATFORMAT_DRIVER_PROJECT) \
		-p \
		-f ;
ifneq ($(BUILD_MODE),dev)
	strip "$(PREFIX)/bin/"*
endif

.PHONY: partial-gnatformat
partial-gnatformat: partial-gnatformat-coverage-instrumentation
	gprbuild \
		-P$(PARTIAL_GNATFORMAT_DRIVER_PROJECT) \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		$(COMMON_BUILD_FLAGS) ;

.PHONY: install-partial-gnatformat
install-partial-gnatformat:
	gprinstall \
		-XGNATFORMAT_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XGNATFORMAT_BUILD_MODE=$(BUILD_MODE) \
		--prefix="$(PREFIX)" \
		--install-name=partial_gnatformat \
		--mode=usage \
		-P$(PARTIAL_GNATFORMAT_DRIVER_PROJECT) \
		-p \
		-f ;
ifneq ($(COVERAGE),)
	mkdir -p $(PREFIX)/share/gnatformat/sids || true
	cp testsuite/partial_gnatformat/obj/*.sid $(PREFIX)/share/gnatformat/sids/
endif

.PHONY: test
test:
	python testsuite/testsuite.py

.PHONY: clean
clean:
	rm -rf bin;
	rm -rf lib;
	rm -rf obj;
	rm -rf testsuite/partial_gnatformat/obj;
	rm -rf testsuite/partial_gnatformat/bin;

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
		-P $(GNATFORMAT_DRIVER_PROJECT) \
		$(COMMON_INSTRUMENT_FLAGS) \
		--projects=gnatformat_driver ;
endif

.PHONY: partial-gnatformat-coverage-instrumentation
partial-gnatformat-coverage-instrumentation:
ifneq ($(COVERAGE),)
	rm -rf testsuite/partial_gnatformat/obj/*gnatcov-instr
	rm -rf testsuite/partial_gnatformat/obj/*/*gnatcov-instr
	gnatcov \
		instrument \
		-P $(PARTIAL_GNATFORMAT_DRIVER_PROJECT) \
		$(COMMON_INSTRUMENT_FLAGS) \
		--projects=partial_gnatformat ;
endif

.PHONY: coverage-run
coverage-run: bin partial-gnatformat
ifneq ($(COVERAGE),)
	python testsuite/testsuite.py --gnatcov obj/ obj/$(LIBRARY_TYPE).$(BUILD_MODE) testsuite/partial_gnatformat/obj --gnatcov-source-root $(ROOT_DIR)
endif
