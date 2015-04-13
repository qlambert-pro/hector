# Copyright 2013, Inria
# Suman Saha, Julia Lawall, Gilles Muller
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


##############################################################################
# Variables
##############################################################################
#TARGET=matcher
-include ../Makefile.config

TARGET=interproc

SRC=interproc.ml

INCLUDES = -I ../commons -I ../commons/ocamlextra -I ../globals -I ../parsing_c -I ./ \
           -I ../definition \
           -I ../errorhandling



LIBS=../commons/commons.cma ../globals/globals.cma \
     ../parsing_c/parsing_c.cma \
     ../definition/definition.cma \
     ../errorhandling/errorhandling.cma

SYSLIBS= str.cma unix.cma


##############################################################################
# Generic variables
##############################################################################

#for warning:  -w A
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLCFLAGS ?= -g -dtypes

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX=ocamllex$(OPTBIN) #-ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN) $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)


OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: intf $(TARGET).cma
all.opt: $(TARGET).cmxa
intf : 
	$(OCAMLC) -c -I ../commons -I ../parsing_c 

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top



##############################################################################
# Pad's rules
##############################################################################

##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
	rm -f *~ .*~ gmon.out #*#

distclean::
	rm -f .depend

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend
