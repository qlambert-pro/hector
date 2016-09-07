# Copyright 2013-2016, Inria
# Suman Saha, Julia Lawall, Gilles Muller, Quentin Lambert
# This file is part of Hector.
#
# Hector is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Hector is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Hector.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Hector under other licenses.


#############################################################################
# Configuration section
#############################################################################



##############################################################################
# Variables
##############################################################################

KAPUTT_FLAGS=
KAPUTT_LIBS=
KAPUTT_INCLUDE=
KAPUTT_DEP=

TOOLS_INCLUDE=

FILTER_TARGET=filter
PRINT_GRAPH_TARGET=print_graph

TARGET=hector

CORE_SRC=ast_operations.ml configs.ml graph_operations.ml annotated_cfg.ml\
	 acfg_operations.ml hector_core.ml

FILTER_SRC=tools/filter_using_error_handling.ml
PRINT_GRAPH_SRC=tools/cocci_addon.ml tools/print_graph.ml

SRCI=ast_operations.mli configs.mli graph_operations.mli annotated_cfg.mli\
     acfg_operations.mli hector_core.mli c_function.mli analyzer.mli

TEST=hector_core.mlt c_function.mlt

HECTOR_SRC=c_function.ml report.ml analyzer.ml
MAIN=main.ml

SYSLIBS=str.cma unix.cma bigarray.cma nums.cma $(KAPUTT_LIBS)
LIBS=coccinelle/commons/commons.cma coccinelle/globals/globals.cma\
     coccinelle/parsing_cocci/cocci_parser.cma\
     coccinelle/parsing_c/parsing_c.cma

#used for clean: and depend: and a little for rec & rec.opt
MAKESUBDIRS=coccinelle/commons coccinelle/globals coccinelle/parsing_c
INCLUDEDIRS=coccinelle/commons coccinelle/commons/ocamlextra\
	    coccinelle/globals coccinelle/parsing_c $(KAPUTT_INCLUDE)\
	    $(TOOLS_INCLUDE)

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

CORE_OBJS       = $(CORE_SRC:.ml=.cmo)
HECTOR_OBJS     = $(HECTOR_SRC:.ml=.cmo)
MAIN_OBJ        = $(MAIN:.ml=.cmo)
FILTER_OBJ      = $(FILTER_SRC:%.ml=%.cmo)
PRINT_GRAPH_OBJ = $(PRINT_GRAPH_SRC:%.ml=%.cmo)

OPT_CORE_OBJS   = $(CORE_SRC:.ml=.cmx)
OPT_HECTOR_OBJS = $(HECTOR_SRC:.ml=.cmx)
MAIN_OPTOBJS = $(MAIN:.ml=.cmx)

EXEC=$(TARGET)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLCFLAGS= -g -w +32# -dtypes # -w A

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
# To enable backtrace support for native code, you need to put -g in OPTFLAGS
# to also link with -g, but even in 3.11 the backtrace support seems buggy so
# not worth it.
OPTFLAGS=
# the following is essential for Coccinelle to compile under gentoo
# but is now defined above in this file

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES) $(KAPUTT_FLAGS)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES) $(KAPUTT_FLAGS)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)


##############################################################################
# Top rules
##############################################################################
.PHONY:: default depend all byte opt top clean realclean

default: byte

all: byte opt

byte: $(EXEC)

opt:  $(EXEC).opt

top:  $(EXEC).top

test: KAPUTT_PATH=$(shell ocamlfind query kaputt)
test: KAPUTT_FLAGS= -pp '$(KAPUTT_PATH)/kaputt_pp.byte on camlp4o'
test: KAPUTT_LIBS= $(KAPUTT_PATH)/kaputt.cma
test: KAPUTT_INCLUDE= $(KAPUTT_PATH)
test: KAPUTT_DEP= $(TEST)
test: $(TEST) $(LIBS) $(CORE_OBJS) $(HECTOR_OBJS)
	@echo "[L] test"
	@$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $(LIBS) $(CORE_OBJS) $(HECTOR_OBJS)

coccinelle/configure:
	@echo "Coccinelle: ./autogen"
	@cd coccinelle && ./autogen

coccinelle/Makefile.config: coccinelle/configure
	@echo "Coccinelle: ./configure"
	@cd coccinelle && ./configure

$(LIBS): coccinelle/configure coccinelle/Makefile.config
	@$(MAKE) -C coccinelle OCAMLCFLAGS="$(OCAMLCFLAGS)" $(@:coccinelle/%=%)

$(LIBS:.cma=.cmxa): coccinelle/configure coccinelle/Makefile.config
	@$(MAKE) -C coccinelle OCAMLCFLAGS="$(OCAMLCFLAGS)" $(@:coccinelle/%=%)

$(CORE_OBJS):$(LIBS)
$(HECTOR_OBJS):$(LIBS)
$(OPT_CORE_OBJS):$(LIBS:.cma=.cmxa)
$(OPT_HECTOR_OBJS):$(LIBS:.cma=.cmxa)

$(EXEC): $(LIBS) $(CORE_OBJS) $(HECTOR_OBJS) $(MAIN_OBJ)
	@echo "[L] hector"
	@$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(FILTER_TARGET): TOOLS_INCLUDE=tools
$(FILTER_TARGET): $(LIBS) $(CORE_OBJS) $(FILTER_OBJ)
	@echo "[L] filter"
	@$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(PRINT_GRAPH_TARGET): TOOLS_INCLUDE=tools
$(PRINT_GRAPH_TARGET): $(LIBS) $(CORE_OBJS) $(PRINT_GRAPH_OBJ)
	@echo "[L] print_graph"
	@$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^


$(EXEC).opt: $(LIBS:.cma=.cmxa) $(OPT_CORE_OBJS) $(OPT_HECTOR_OBJS) $(MAIN_OPTOBJS)
	@$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

$(EXEC).top: $(LIBS) $(CORE_OBJS) $(HECTOR_OBJS) $(MAIN_OBJ)
	@$(OCAMLMKTOP) -custom -o $@ $(SYSLIBS) $^

##############################################################################
# Generic ocaml rules
##############################################################################

%.cmo: %.ml %.ml.d
	@echo "[C] $<"
	@$(OCAMLC)    -c $<

%.cmi: %.mli %.mli.d
	@echo "[C] $<"
	@$(OCAMLC)    -c $<

%.cmx: %.ml
	@$(OCAMLOPT)  -c $<

%.ml.d:: %.ml %.mlt
	@$(OCAMLDEP) $< > $@
	@echo $(<:.ml=.cmo) : $(<:.ml=.mlt) >> $@
	@echo $(<:.ml=.cmx) : $(<:.ml=.mlt) >> $@

%.ml.d:: %.ml
	@$(OCAMLDEP) $< > $@

tools/%.ml.d:: tools/%.ml
	@$(OCAMLDEP) $< > $@

%.mli.d: %.mli
	@$(OCAMLDEP) $< > $@

clean:
	@rm -f $(FILTER_TARGET) test $(TARGET) $(TARGET).opt $(TARGET).top
	@rm -f $(PRINT_GRAPH_TARGET)
	@rm -f *.d
	@rm -f tools/*.d
	@rm -f *.cm[iox] *.o *.annot
	@rm -f tools/*.cm[iox] tools/*.o
	@rm -f *~ *.exe

realclean: clean
	make -C coccinelle clean

-include $(MAIN:.ml=.ml.d) $(CORE_SRC:.ml=.ml.d) $(HECTOR_SRC:.ml=.ml.d)\
	$(SRCI:.mli=.mli.d) $(FILTER_SRC:.ml=.ml.d)	$(PRINT_GRAPH_SRC:.ml=.ml.d)
