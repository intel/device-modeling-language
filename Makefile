# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

#
# makefile for dmlc
#

DMLC_DIR := $(SRC_BASE)/$(TARGET)
LIBDIR := $(SIMICS_PROJECT)/$(HOST_TYPE)/bin

PYFILES := dml/__init__.py			\
	  dml/ast.py				\
	  dml/c_backend.py			\
	  dml/g_backend.py			\
	  dml/codegen.py			\
          dml/crep.py				\
          dml/ctree.py				\
          dml/template.py			\
          dml/toplevel.py			\
          dml/traits.py				\
          dml/types.py				\
          dml/dmllex.py				\
          dml/dmllex12.py			\
          dml/dmllex14.py			\
          dml/dmlparse.py			\
          dml/dmlc.py				\
          dml/expr.py				\
          dml/expr_util.py			\
	  dml/globals.py			\
          dml/info_backend.py			\
          dml/io_memory.py			\
          dml/int_register.py			\
          dml/logging.py			\
          dml/messages.py			\
          dml/objects.py			\
          dml/output.py				\
	  dml/reginfo.py			\
          dml/serialize.py				\
          dml/slotsmeta.py			\
          dml/structure.py			\
          dml/symtab.py				\
	  __main__.py

PYUNIT_FILES := $(wildcard $(DMLC_DIR)/py/dml/*_test.py)
PYUNIT_TESTED := $(foreach x,$(PYUNIT_FILES),$(basename $(notdir $x))-pyunit-tested)

GEN_PYFILES :=	dml12_parsetab.py \
		dml14_parsetab.py

.SECONDARY: $(GEN_PYFILES)

PYTHONPATH = $(LIBDIR)/dml/python

OUT_PYFILES     := $(addprefix $(PYTHONPATH)/,$(PYFILES))
OUT_GEN_PYFILES := $(addprefix $(PYTHONPATH)/dml/,$(GEN_PYFILES))

# used later on by refmanual build
PARSER_DEBUGFILES :=	$(PYTHONPATH)/dml12_parser.out \
			$(PYTHONPATH)/dml14_parser.out

include $(SIMICS_BASE)/$(HOST_TYPE)/include/api-versions.mk

# lib-old-4.8/* is copied to bin/dml-old-4.8
# lib/* is copied to both bin/dml, bin/dml-old-4.8
OLD_DMLLIB_SRC_4_8 := $(DMLC_DIR)/lib-old-4.8
OLD_DMLLIB_DEST_4_8 := $(LIBDIR)/dml-old-4.8
DMLLIB_SRC := $(DMLC_DIR)/lib
DMLLIB_DEST := $(LIBDIR)/dml
DMLLIB_DESTDIRS := $(addprefix $(DMLLIB_DEST)/,1.2 1.4)
OLD_DMLLIB_DESTDIRS_4_8 := $(addprefix $(OLD_DMLLIB_DEST_4_8)/,1.2 1.4)
OLD_DMLFILES_4_8 := $(wildcard $(OLD_DMLLIB_SRC_4_8)/*/*.dml)
DMLFILES := $(wildcard $(DMLLIB_SRC)/*/*.dml)
OLD_DMLFILES_SRC_4_8 := $(subst $(OLD_DMLLIB_SRC_4_8)/,,$(OLD_DMLFILES_4_8))
DMLFILES := $(subst $(DMLLIB_SRC)/,,$(DMLFILES))
OLD_DMLFILES_4_8 := $(addprefix $(OLD_DMLLIB_DEST_4_8)/,$(DMLFILES) $(OLD_DMLFILES_SRC_4_8))
DMLFILES := $(addprefix $(DMLLIB_DEST)/,$(DMLFILES))
SCRIPTS := $(PYTHONPATH)/port_dml.py
MPL_LICENSE := $(PYTHONPATH)/LICENSE
BSD0_LICENSES := $(addsuffix /LICENSE,$(DMLLIB_DESTDIRS) $(OLD_DMLLIB_DESTDIRS_4_8) $(DMLLIB_DEST)/include/simics)

HFILES := $(DMLLIB_DEST)/include/simics/dmllib.h

DMLC_BIN := $(OUT_PYFILES) $(OUT_GEN_PYFILES) $(HFILES)

all: $(DMLC_BIN)					\
     $(SCRIPTS)						\
     $(DMLFILES)					\
     $(OLD_DMLFILES_4_8)				\
     $(BSD0_LICENSES)					\
     $(MPL_LICENSE)					\
     $(PYUNIT_TESTED)					\
     $(PARSER_DEBUGFILES)

EXE_SUFFIX := $(if $(findstring win,$(HOST_TYPE)),.exe,)
DMLC_CMD := $(PYTHON) $(PYTHONPATH)/__main__.py

PYCOMPILE:=$(PYTHON) $(SIMICS_BASE)/scripts/copy_python.py --compile

$(OUT_GEN_PYFILES): $(LIBDIR)/dml/python/dml/%.py: %.py
	$(info Compiling $(<F))
	$(PYCOMPILE) $^ $@

$(OUT_PYFILES): $(LIBDIR)/dml/python/%.py: $(DMLC_DIR)/py/%.py
	$(info Compiling $(<F))
	$(PYCOMPILE) $^ $@

$(SCRIPTS): $(PYTHONPATH)/%: $(DMLC_DIR)/py/%
	cp $< $@

$(HFILES): $(DMLLIB_DEST)/%: $(DMLC_DIR)/%
	$(PYTHON) $(DMLC_DIR)/copy_h.py $< $@

# Generate the parser tables
# Fairly expensive, so we only rebuild when relevant files have
# changed
# We would want to keep the parsetabs in the dml Python package, but
# that cannot be done because of ply (which assumes it's a top-level module)
dml%_parsetab.py $(PYTHONPATH)/dml%_parser.out: \
    $(SRC_BASE)/$(TARGET)/generate_parsetabs.py \
    $(filter %dmlparse.py %dmllex.py %dmllex12.py %dmllex14.py %ast.py, \
      $(OUT_PYFILES)) \
    | $(OUT_PYFILES) $(filter-out %parsetab.py,$(OUT_GEN_PYFILES))
	$(info Generating $(@F))
	$(PYTHON) $< $(PYTHONPATH) $* dml$*_parsetab $(PYTHONPATH)/dml$*_parser.out

# needed by ctree_test.py
export CC
export SIMICS_BASE

RUN_PY_UNIT_TEST := $(PYTHON) $(DMLC_DIR)/run_unit_tests.py
%-pyunit-tested: $(DMLC_DIR)/py/dml/%.py $(OUT_PYFILES)
	$(info Testing $<)
	$(RUN_PY_UNIT_TEST) $(SIMICS_PROJECT)/$(HOST_TYPE) $<
	touch $@

$(DMLLIB_DESTDIRS) $(OLD_DMLLIB_DESTDIRS_4_8) $(PYTHONPATH):
	$(MKDIRS) $@

# Copy the DML library files
$(DMLFILES): $(DMLLIB_DEST)/%: $(DMLLIB_SRC)/% | $(DMLLIB_DESTDIRS)
	$(info Copying $*)
	cp $< $@

$(OLD_DMLLIB_DEST_4_8)/%: $(DMLLIB_SRC)/% | $(OLD_DMLLIB_DESTDIRS_4_8)
	$(info Copying $*)
	cp $< $@

$(OLD_DMLLIB_DEST_4_8)/%: $(OLD_DMLLIB_SRC_4_8)/% | $(OLD_DMLLIB_DESTDIRS_4_8)
	$(info Copying $*)
	cp $< $@

$(MPL_LICENSE): $(PYTHONPATH)/%: $(DMLC_DIR)/% | $(PYTHONPATH)
	$(info Copying $*)
	cp $< $@

$(DMLLIB_DESTDIRS:=/LICENSE): $(DMLLIB_DEST)/%: $(DMLC_DIR)/lib/% | $(DMLLIB_DESTDIRS)
	$(info Copying $*)
	cp $< $@
$(OLD_DMLLIB_DESTDIRS_4_8:=/LICENSE): $(DMLC_DIR)/lib-old-4.8/1.2/LICENSE | $(OLD_DMLLIB_DESTDIRS_4_8)
	$(info Copying $*)
	cp $< $@
# HFILES dep to have the dest dir created
$(DMLLIB_DEST)/include/simics/LICENSE: $(DMLLIB_DEST)/%: $(DMLC_DIR)/% | $(HFILES)
	$(info Copying $*)
	cp $< $@

# Rules that depend on this marker file can rely on dmlc to be built,
# and will be rebuilt when the parser has changed.
dmlast-generator: $(DMLC_BIN)
	$(if $(filter %_parsetab.py %/dmlc.py,$?),touch $@)

# New .dmlast files are generated for all .dml files that changed
# since last time.
# Makefile contraption: the files <build-dir>/x/y/z.d are both markers and
# depfiles. For each such marker, we run dmlc with a --save-asts that
# both pre-compiles all files in <bin-dir>/x/y/z, and creates the
# marker. The marker doubles as a depfile that makes sure we rebuild
# this directory when DML files are updated
PREPARSE_MARKERS := $(foreach d,1.2 1.4,\
  dml-old-4.8/$d.d dml/$d.d $(foreach a,$(API_VERSIONS),dml/api/$a/$d.d))
-include $(PREPARSE_MARKERS)
DML_VERSIONS:=1.2 1.4
$(DML_VERSIONS:%=dml-old-4.8/%.d): $(OLD_DMLFILES_4_8)
$(DML_VERSIONS:%=dml/%.d): $(DMLFILES)
DMLAST_SRC_BASE:=$(if $(_CORE_PROJECT_BUILD),$(SIMICS_PROJECT),$(SIMICS_BASE))
$(foreach d,1.2 1.4,dml-old-4.8/$d.d dml/$d.d): DMLAST_SRC_BASE:=$(SIMICS_PROJECT)
$(PREPARSE_MARKERS): dmlast-generator
	$(info Generating .dmlast files for $(@:.d=))
	$(PYTHON) $(SRC_BASE)/$(TARGET)/dmlast.py $(PYTHONPATH) $(SIMICS_PROJECT)/$(HOST_TYPE)/bin/$(@:.d=) $(DMLAST_SRC_BASE)/$(HOST_TYPE)/bin/$(@:.d=) $@
all: $(PREPARSE_MARKERS)

DODOC := $(SIMICS_BASE)/$(HOST_TYPE)/bin/dodoc$(EXE_SUFFIX)

generated-md-1.2 generated-md-1.4:
	$(MKDIRS) $@
generated-md-1.2/grammar.md generated-md-1.4/grammar.md: generated-md-1.%/grammar.md: $(PYTHONPATH)/dml1%_parser.out $(SRC_BASE)/$(TARGET)/grammar_to_md.py
	$(PYTHON) $(SRC_BASE)/$(TARGET)/grammar_to_md.py $(PYTHONPATH) $< $@
generated-md-1.2/messages.md generated-md-1.4/messages.md: generated-md-%/messages.md: $(SRC_BASE)/$(TARGET)/messages_to_md.py $(DMLC_BIN)
	$(PYTHON) $< $(PYTHONPATH) $* $@

DOC_SRC_DIR_14 := $(SRC_BASE)/$(TARGET)/doc/1.4
DOC_FILES_14 := $(wildcard $(DOC_SRC_DIR_14)/*.md) $(DOC_SRC_DIR_14)/toc.json

DOC_DEST_14 := $(SIMICS_PROJECT)/$(HOST_TYPE)/doc/html/dml-1.4-reference-manual
DOC_MARKER_14 := $(DOC_DEST_14)/filelist.json

GENERATED_MD_FILES_14 = $(addprefix generated-md-1.4/,grammar.md messages.md changes-auto.md dml-builtins.md utility.md)
generated-md-1.4/changes-auto.md: $(SRC_BASE)/$(TARGET)/porting_to_md.py $(DMLC_BIN) | generated-md-1.4
	$(PYTHON) $< $(PYTHONPATH) $@
generated-md-1.4/dml-builtins.md generated-md-1.4/utility.md: generated-md-1.4/%.md: $(DMLC_DIR)/lib/1.4/%.dml
	$(PYTHON) $(DMLC_DIR)/dmlcomments_to_md.py $< $@
DOC_FILES_14 += $(GENERATED_MD_FILES_14)
$(GENERATED_MD_FILES_14): | generated-md-1.4
$(DOC_MARKER_14): $(DOC_FILES_14)
	$(PYTHON) $(DMLC_DIR)/validate_md_links.py $(DOC_SRC_DIR_14)/toc.json $(DOC_SRC_DIR_14) generated-md-1.4
	$(DODOC) --css simics.css $(SIMICS_BASE)/src/docs/dodoc -o $(DOC_DEST_14) $(DOC_SRC_DIR_14) generated-md-1.4

all: $(DOC_MARKER_14)

DOC_SRC_DIR_12 := $(SRC_BASE)/$(TARGET)/doc/1.2
DOC_FILES_12 := $(wildcard $(DOC_SRC_DIR_12)/*.md) $(DOC_SRC_DIR_12)/toc.json

DOC_DEST_12 := $(SIMICS_PROJECT)/$(HOST_TYPE)/doc/html/dml-1.2-reference-manual
DOC_MARKER_12 := $(DOC_DEST_12)/filelist.json

GENERATED_MD_FILES_12 = $(addprefix generated-md-1.2/,grammar.md messages.md)
DOC_FILES_12 += $(GENERATED_MD_FILES_12)
$(GENERATED_MD_FILES_12): | generated-md-1.2
$(DOC_MARKER_12): $(DOC_FILES_12)
	$(PYTHON) $(DMLC_DIR)/validate_md_links.py $(DOC_SRC_DIR_12)/toc.json $(DOC_SRC_DIR_12) generated-md-1.2
	$(DODOC) --css simics.css $(SIMICS_BASE)/src/docs/dodoc -o $(DOC_DEST_12) $(DOC_SRC_DIR_12) generated-md-1.2

all: $(DOC_MARKER_12)
