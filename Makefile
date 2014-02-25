#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# The main Makefile

include config/Makefile
include stdlib/StdlibModules

include Makefile_variables.boot
include Makefile_variables.byte
include Makefile_variables.opt

COMMON_COMPFLAGS=-strict-sequence -w +33..39 -warn-error A

# CAMLC=boot/ocamlrun boot/ocamlc -nostdlib -I stdlib
# CAMLOPT=boot/ocamlrun ./ocamlopt -nostdlib -I stdlib -I otherlibs/dynlink
# COMPFLAGS=$(COMMON_COMPFLAGS) $(INCLUDES)
LINKFLAGS=

BOOT_CAMLC=boot/ocamlrun boot/ocamlc -nostdlib -I stdlib/boot
BOOT_COMPFLAGS=$(COMMON_COMPFLAGS) $(BOOT_INCLUDES)

BYTE_CAMLC=byterun/ocamlrun boot_build/ocamlc -nostdlib -I stdlib/byte
BYTE_CAMLOPT=byterun/ocamlrun boot_build/ocamlopt -nostdlib -I stdlib/byte -I otherlibs/dynlink
BYTE_COMPFLAGS=$(COMMON_COMPFLAGS) $(BYTE_INCLUDES)

OPT_CAMLC=byte/ocamlc.opt -nostdlib -I stdlib/opt
OPT_CAMLOPT=byte/ocamlopt.opt -nostdlib -I stdlib/opt -I otherlibs/dynlink
OPT_COMPFLAGS=$(COMMON_COMPFLAGS) $(OPT_INCLUDES)

CAMLYACC=boot/ocamlyacc
YACCFLAGS=-v
CAMLLEX=boot/ocamlrun boot/ocamllex
CAMLDEP=boot/ocamlrun tools/ocamldep
TEMPLATER=boot/ocamlrun tools/make_templater
CAMLRUN=byterun/ocamlrun
SHELL=/bin/sh
MKDIR=mkdir -p

OCAMLBUILDBYTE=$(WITH_OCAMLBUILD:=.byte)
OCAMLBUILDNATIVE=$(WITH_OCAMLBUILD:=.native)

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)

DEPFLAGS=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver \
	 -I toplevel -I tools

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

# For users who don't read the INSTALL file
defaultentry:
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world.opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."

# Recompile the system using the bootstrap compiler
# all:
# 	$(MAKE) runtime
# 	$(MAKE) ocamlc
# 	$(MAKE) ocamllex
# 	$(MAKE) ocamlyacc
# 	$(MAKE) ocamltools
# 	$(MAKE) library
# 	$(MAKE) ocaml
# 	$(MAKE) otherlibraries $(OCAMLBUILDBYTE) $(WITH_DEBUGGER) \
# 	  $(WITH_OCAMLDOC)

# Compile everything the first time
# world:
# 	$(MAKE) coldstart
# 	$(MAKE) all

# Compile also native code compiler and libraries, fast
# world.opt:
# 	$(MAKE) coldstart
# 	$(MAKE) opt.opt

# Hard bootstrap how-to:
# (only necessary in some cases, for example if you remove some primitive)
#
# make coreboot     [old system -- you were in a stable state]
# <change the source>
# make core         [cross-compiler]
# make partialclean [if you get "inconsistent assumptions"]
# <debug your changes>
# make core         [cross-compiler]
# make coreboot     [new system -- now you are in a stable state]

# # Core bootstrapping cycle
# # coreboot:
# # Save the original bootstrap compiler
# 	$(MAKE) backup
# # Promote the new compiler but keep the old runtime
# # This compiler runs on boot/ocamlrun and produces bytecode for
# # byterun/ocamlrun
# 	$(MAKE) promote-cross
# # Rebuild ocamlc and ocamllex (run on byterun/ocamlrun)
# 	$(MAKE) partialclean
# 	$(MAKE) ocamlc ocamllex ocamltools
# # Rebuild the library (using byterun/ocamlrun ./ocamlc)
# 	$(MAKE) library-cross
# # Promote the new compiler and the new runtime
# 	$(MAKE) promote
# # Rebuild the core system
# 	$(MAKE) partialclean
# 	$(MAKE) core
# # Check if fixpoint reached
# 	$(MAKE) compare

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
# bootstrap:
# 	$(MAKE) coreboot
# 	$(MAKE) all
# 	$(MAKE) compare

# LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

# Start up the system from the distribution compiler
# coldstart:
# 	cd byterun; $(MAKE) all
# 	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)
# 	cd yacc; $(MAKE) all
# 	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
# 	cd stdlib; $(MAKE) COMPILER=../boot/ocamlc all
# 	cd stdlib; cp $(LIBFILES) ../boot
# 	if test -f boot/libcamlrun.a; then :; else \
# 	  ln -s ../byterun/libcamlrun.a boot/libcamlrun.a; fi
# 	if test -d stdlib/caml; then :; else \
# 	  ln -s ../byterun stdlib/caml; fi


asmrun/libasmrun.a:
	$(MAKE) -C asmrun libasmrun.a

byterun/ocamlrun:
	$(MAKE) -C byterun ocamlrun

# Build the core system: the minimum needed to make depend and bootstrap
# core:
# 	$(MAKE) coldstart
# 	$(MAKE) ocamlc
# 	$(MAKE) ocamllex ocamlyacc ocamltools library

# Recompile the core system using the bootstrap compiler
# coreall:
# 	$(MAKE) ocamlc
# 	$(MAKE) ocamllex ocamlyacc ocamltools library

# Save the current bootstrap compiler
MAXSAVED=boot/Saved/Saved.prev/Saved.prev/Saved.prev/Saved.prev/Saved.prev
backup:
	if test -d boot/Saved; then : ; else mkdir boot/Saved; fi
	if test -d $(MAXSAVED); then rm -r $(MAXSAVED); else : ; fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/ocamlrun$(EXE) boot/Saved
	mv boot/ocamlc boot/ocamllex boot/ocamlyacc$(EXE) boot/ocamldep \
	   boot/Saved
	cd boot; cp $(LIBFILES) Saved

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
promote-cross:
	cp ocamlc boot/ocamlc
	cp lex/ocamllex boot/ocamllex
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	cp tools/ocamldep boot/ocamldep
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
promote: promote-cross
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Restore the saved bootstrap compiler if a problem arises
restore:
	mv boot/Saved/* boot
	rmdir boot/Saved
	mv boot/Saved.prev boot/Saved

# Check if fixpoint reached
compare:
	@if cmp boot/ocamlc ocamlc && cmp boot/ocamllex lex/ocamllex \
	    && cmp boot/ocamldep tools/ocamldep; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Remove old bootstrap compilers
cleanboot:
	rm -rf boot/Saved/Saved.prev/*

# Compile the native-code compiler
# opt-core:
# 	$(MAKE) runtimeopt
# 	$(MAKE) ocamlopt
# 	$(MAKE) libraryopt

# opt:
# 	$(MAKE) runtimeopt
# 	$(MAKE) ocamlopt
# 	$(MAKE) libraryopt
# 	$(MAKE) otherlibrariesopt ocamltoolsopt $(OCAMLBUILDNATIVE)

# Native-code versions of the tools
# opt.opt:
# 	$(MAKE) checkstack
# 	$(MAKE) runtime
# 	$(MAKE) core
# 	$(MAKE) ocaml
# 	$(MAKE) opt-core
# 	$(MAKE) ocamlc.opt
# 	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) \
# 	        $(OCAMLBUILDBYTE)
# 	$(MAKE) ocamlopt.opt
# 	$(MAKE) otherlibrariesopt
# 	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT) \
# 	        $(OCAMLBUILDNATIVE)

# base.opt:
# 	$(MAKE) checkstack
# 	$(MAKE) runtime
# 	$(MAKE) core
# 	$(MAKE) ocaml
# 	$(MAKE) opt-core
# 	$(MAKE) ocamlc.opt
# 	$(MAKE) otherlibraries $(OCAMLBUILDBYTE) $(WITH_DEBUGGER) \
# 	  $(WITH_OCAMLDOC)
# 	$(MAKE) ocamlopt.opt
# 	$(MAKE) otherlibrariesopt

# Installation

COMPLIBDIR=$(LIBDIR)/compiler-libs

install:
	if test -d $(BINDIR); then : ; else $(MKDIR) $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else $(MKDIR) $(LIBDIR); fi
	if test -d $(STUBLIBDIR); then : ; else $(MKDIR) $(STUBLIBDIR); fi
	if test -d $(COMPLIBDIR); then : ; else $(MKDIR) $(COMPLIBDIR); fi
	if test -d $(MANDIR)/man$(MANEXT); then : ; \
	  else $(MKDIR) $(MANDIR)/man$(MANEXT); fi
	cp VERSION $(LIBDIR)/
	cd $(LIBDIR); rm -f dllbigarray.so dllnums.so dllthreads.so \
	  dllunix.so dllgraphics.so dllstr.so
	cd byterun; $(MAKE) install
	cp ocamlc $(BINDIR)/ocamlc$(EXE)
	cp ocaml $(BINDIR)/ocaml$(EXE)
	cd stdlib; $(MAKE) install
	cp lex/ocamllex $(BINDIR)/ocamllex$(EXE)
	cp yacc/ocamlyacc$(EXE) $(BINDIR)/ocamlyacc$(EXE)
	cp utils/*.cmi parsing/*.cmi typing/*.cmi bytecomp/*.cmi driver/*.cmi \
	   toplevel/*.cmi $(COMPLIBDIR)
	cp compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
	   compilerlibs/ocamltoplevel.cma $(BYTESTART) $(TOPLEVELSTART) \
	   $(COMPLIBDIR)
	cp expunge $(LIBDIR)/expunge$(EXE)
	cp toplevel/topdirs.cmi $(LIBDIR)
	cd tools; $(MAKE) install
	-cd man; $(MAKE) install
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) install) || exit $$?; \
	done
	if test -n "$(WITH_OCAMLDOC)"; then (cd ocamldoc; $(MAKE) install); else :; fi
	if test -n "$(WITH_DEBUGGER)"; then (cd debugger; $(MAKE) install); \
	   else :; fi
	if test -n "$(WITH_OCAMLBUILD)"; then (cd ocamlbuild; $(MAKE) install); \
	   else :; fi
	cp config/Makefile $(LIBDIR)/Makefile.config
	if test -f ocamlopt; then $(MAKE) installopt; else :; fi

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp ocamlopt $(BINDIR)/ocamlopt$(EXE)
	cd stdlib; $(MAKE) installopt
	cp asmcomp/*.cmi $(COMPLIBDIR)
	cp compilerlibs/ocamloptcomp.cma $(OPTSTART) $(COMPLIBDIR)
	if test -n "$(WITH_OCAMLDOC)"; then (cd ocamldoc; $(MAKE) installopt); \
		else :; fi
	if test -n "$(WITH_OCAMLBUILD)"; then (cd ocamlbuild; $(MAKE) installopt); \
	   else :; fi
	for i in $(OTHERLIBRARIES); \
	  do (cd otherlibs/$$i; $(MAKE) installopt) || exit $$?; done
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; fi
	cd tools; $(MAKE) installopt

installoptopt:
	cp ocamlc.opt $(BINDIR)/ocamlc.opt$(EXE)
	cp ocamlopt.opt $(BINDIR)/ocamlopt.opt$(EXE)
	cp lex/ocamllex.opt $(BINDIR)/ocamllex.opt$(EXE)
	cp compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlcommon.a \
	   compilerlibs/ocamlbytecomp.cmxa compilerlibs/ocamlbytecomp.a \
	   compilerlibs/ocamloptcomp.cmxa compilerlibs/ocamloptcomp.a \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.o) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.o) \
	   $(COMPLIBDIR)
	cd $(COMPLIBDIR) && $(RANLIB) ocamlcommon.a ocamlbytecomp.a \
	   ocamloptcomp.a

clean:: partialclean

# The native toplevel

# ocamlnat: ocamlopt otherlibs/dynlink/dynlink.cmxa $(NATTOPOBJS:.cmo=.cmx)
# 	$(CAMLOPT) $(LINKFLAGS) otherlibs/dynlink/dynlink.cmxa -o ocamlnat \
# 	           $(NATTOPOBJS:.cmo=.cmx) -linkall

# toplevel/opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

# otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/natdynlink.ml
# 	cd otherlibs/dynlink && $(MAKE) allopt

# The configuration file

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
	    -e 's|%%BYTERUN%%|$(BINDIR)/ocamlrun|' \
	    -e 's|%%CCOMPTYPE%%|cc|' \
	    -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
	    -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
	    -e 's|%%PACKLD%%|$(PACKLD)|' \
	    -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
	    -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
	    -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
	    -e 's|%%ARCMD%%|$(ARCMD)|' \
	    -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
	    -e 's|%%ARCH%%|$(ARCH)|' \
	    -e 's|%%MODEL%%|$(MODEL)|' \
	    -e 's|%%SYSTEM%%|$(SYSTEM)|' \
	    -e 's|%%EXT_OBJ%%|.o|' \
	    -e 's|%%EXT_ASM%%|.s|' \
	    -e 's|%%EXT_LIB%%|.a|' \
	    -e 's|%%EXT_DLL%%|.so|' \
	    -e 's|%%SYSTHREAD_SUPPORT%%|$(SYSTHREAD_SUPPORT)|' \
	    -e 's|%%ASM%%|$(ASM)|' \
	    -e 's|%%ASM_CFI_SUPPORTED%%|$(ASM_CFI_SUPPORTED)|' \
	    -e 's|%%WITH_FRAME_POINTERS%%|$(WITH_FRAME_POINTERS)|' \
	    -e 's|%%MKDLL%%|$(MKDLL)|' \
	    -e 's|%%MKEXE%%|$(MKEXE)|' \
	    -e 's|%%MKMAINDLL%%|$(MKMAINDLL)|' \
	    -e 's|%%HOST%%|$(HOST)|' \
	    -e 's|%%TARGET%%|$(TARGET)|' \
	    utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

tools/ocamlmklibconfig.ml: config/Makefile
	(echo 'let bindir = "$(BINDIR)"'; \
         echo 'let ext_lib = "$(EXT_LIB)"'; \
         echo 'let ext_dll = "$(EXT_DLL)"'; \
         echo 'let supports_shared_libraries = $(SUPPORTS_SHARED_LIBRARIES)';\
         echo 'let mkdll = "$(MKDLL)"'; \
         echo 'let byteccrpath = "$(BYTECCRPATH)"'; \
         echo 'let nativeccrpath = "$(NATIVECCRPATH)"'; \
         echo 'let mksharedlibrpath = "$(MKSHAREDLIBRPATH)"'; \
         echo 'let toolpref = "$(TOOLPREF)"'; \
         sed -n -e 's/^#ml //p' config/Makefile) \
        > tools/ocamlmklibconfig.ml

beforedepend:: tools/ocamlmklibconfig.ml

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
	awk -f tools/make-opcodes > bytecomp/opcodes.ml

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
	 echo '|]'; \
	 echo 'let builtin_primitives = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

BOOT_ARCH=$(ARCH)
BYTE_ARCH=$(ARCH)
OPT_ARCH=$(ARCH)

include Makefile_rules.boot
include Makefile_rules.byte
include Makefile_rules.opt

# cvt_emit

# The lexer

tools/cvt_emit.ml: tools/cvt_emit.mll
	$(CAMLLEX) tools/cvt_emit.mll

partialclean::
	rm -f tools/cvt_emit.ml

beforedepend:: tools/cvt_emit.ml

# make templater

tools/make_templater.ml: tools/make_templater.mll
	$(CAMLLEX) tools/make_templater.mll

partialclean::
	rm -f tools/make_templater.ml

beforedepend:: tools/make_templater.ml


# The "expunge" utility

expunge: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
         toplevel/expunge.cmo
	$(CAMLC) $(LINKFLAGS) -o expunge compilerlibs/ocamlcommon.cma \
	         compilerlibs/ocamlbytecomp.cma toplevel/expunge.cmo

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

runtime:
	cd byterun; $(MAKE) all
	if test -f stdlib/libcamlrun.a; then :; else \
	  ln -s ../byterun/libcamlrun.a stdlib/libcamlrun.a; fi

clean::
	cd byterun; $(MAKE) clean
	rm -f stdlib/libcamlrun.a
	rm -f stdlib/caml

alldepend::
	cd byterun; $(MAKE) depend

# The runtime system for the native-code compiler

runtimeopt: makeruntimeopt
	cp asmrun/libasmrun.a stdlib/libasmrun.a

makeruntimeopt:
	cd asmrun; $(MAKE) all

clean::
	cd asmrun; $(MAKE) clean
	rm -f stdlib/libasmrun.a

alldepend::
	cd asmrun; $(MAKE) depend

# The library

library: ocamlc
	cd stdlib; $(MAKE) all

library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/ocamlrun all

libraryopt:
	cd stdlib; $(MAKE) allopt

partialclean::
	cd stdlib; $(MAKE) clean

alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

lex/ocamllex: yacc/ocamlyacc boot_build/ocamlc stdlib/byte/stdlib.cma stdlib/byte/std_exit.cmo
	cd lex; $(MAKE) all

lex/ocamllex.opt: boot_build/ocamlopt stdlib/byte/stdlib.cmxa stdlib/byte/std_exit.cmx
	cd lex; $(MAKE) allopt

partialclean::
	cd lex; $(MAKE) clean

alldepend::
	cd lex; $(MAKE) depend

yacc/ocamlyacc:
	cd yacc; $(MAKE) all

clean::
	cd yacc; $(MAKE) clean

# Tools

# ocamltools: ocamlc ocamlyacc ocamllex asmcomp/cmx_format.cmi
# 	cd tools; $(MAKE) all

# ocamltoolsopt: ocamlopt
# 	cd tools; $(MAKE) opt

# ocamltoolsopt.opt: ocamlc.opt ocamlyacc ocamllex asmcomp/cmx_format.cmi
# 	cd tools; $(MAKE) opt.opt

partialclean::
	cd tools; $(MAKE) clean

alldepend::
	cd tools; $(MAKE) depend

# OCamldoc

ocamldoc/ocamldoc: byte/ocamlc yacc/ocamlyacc lex/ocamllex otherlibraries
	cd ocamldoc && $(MAKE) all

ocamldoc/ocamldoc.opt: ocamlc.opt yacc/ocamlyacc lex/ocamllex.opt
	cd ocamldoc && $(MAKE) opt.opt

partialclean::
	cd ocamldoc && $(MAKE) clean

alldepend::
	cd ocamldoc && $(MAKE) depend

# The extra libraries

otherlibraries: ocamltools
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) RUNTIME=$(RUNTIME) all) || exit $$?; \
	done

otherlibrariesopt:
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i; $(MAKE) allopt) || exit $$?; \
	done

partialclean::
	for i in $(OTHERLIBRARIES); do \
	  (cd otherlibs/$$i && $(MAKE) partialclean); \
	done

clean::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i && $(MAKE) clean); done

alldepend::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) depend); done

# The replay debugger

ocamldebugger: ocamlc ocamlyacc ocamllex otherlibraries
	cd debugger; $(MAKE) all

partialclean::
	cd debugger; $(MAKE) clean

alldepend::
	cd debugger; $(MAKE) depend

# Ocamlbuild

ocamlbuild.byte: ocamlc otherlibraries
	cd ocamlbuild && $(MAKE) all

ocamlbuild.native: ocamlopt otherlibrariesopt
	cd ocamlbuild && $(MAKE) allopt

partialclean::
	cd ocamlbuild && $(MAKE) clean

alldepend::
	cd ocamlbuild && $(MAKE) depend

# Check that the stack limit is reasonable.

checkstack:
	@if $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) \
	              -o tools/checkstack tools/checkstack.c; \
	  then tools/checkstack; \
	  else :; \
	fi
	@rm -f tools/checkstack

# Make clean in the test suite

clean::
	cd testsuite; $(MAKE) clean

# Make MacOS X package

package-macosx:
	sudo rm -rf package-macosx/root
	$(MAKE) PREFIX="`pwd`"/package-macosx/root install
	tools/make-package-macosx
	sudo rm -rf package-macosx/root

clean::
	rm -rf package-macosx/*.pkg package-macosx/*.dmg

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

# per directory variables

boot_build/asmcomp/boot: | asmcomp/boot
byte/asmcomp/byte: | asmcomp/byte
opt/asmcomp/opt: | asmcomp/opt

ALL= $(sort $(BOOT_ALL) $(BYTE_ALL) $(OPT_ALL))

# $(sort $(ALL) $(ALL:.cmo=.cmi)): stdlib/$$(STDLIB_DIR)/stdlib.cma
# $(sort $(ALL:.cmo=.cmx)): stdlib/$$(STDLIB_DIR)/stdlib.cmxa

# $(sort $(BOOT_ALL) $(BOOT_ALL:.cmo=.cmi)): boot/ocamlc
# $(sort $(BOOT_ALL:.cmo=.cmx)): impossible_to_build

# $(sort $(BYTE_ALL) $(BYTE_ALL:.cmo=.cmi)): boot_build/ocamlc
# $(sort $(BYTE_ALL:.cmo=.cmx)): boot_build/ocamlopt

# $(sort $(OPT_ALL) $(OPT_ALL:.cmo=.cmi)): byte/ocamlc.opt
# $(sort $(OPT_ALL:.cmo=.cmx)): byte/ocamlopt.opt

boot_build/tools/cvt_emit: $(BOOT_CVT_EMIT) stdlib/boot/std_exit.cmo
	$(BOOT_CAMLC) $(LINKFLAGS) -o $@ $(BOOT_CVT_EMIT)

boot_build/tools/make_templater: $(BOOT_MAKE_TEMPLATER) stdlib/boot/std_exit.cmo | boot_build/tools
	$(BOOT_CAMLC) $(LINKFLAGS) -o $@ $(BOOT_MAKE_TEMPLATER)

boot_build/tools/ocamlmklib: $(BOOT_MKLIB) stdlib/boot/std_exit.cmo
	$(BOOT_CAMLC) $(LINKFLAGS) -o $@ $(BOOT_MKLIB)

boot_build/tools/ocamldep: $(BOOT_CAMLDEP_IMPORTS) $(BOOT_CAMLDEP_OBJ) stdlib/boot/std_exit.cmo
	$(BOOT_CAMLC) $(LINKFLAGS) -compat-32 -o $@ \
	              $(BOOT_CAMLDEP_IMPORTS) $(BOOT_CAMLDEP_OBJ)

tools/make_templater: boot_build/tools/make_templater
	cp boot_build/tools/make_templater $@

tools/ocamldep: boot_build/tools/ocamldep
	ln -s ../boot_build/tools/ocamldep tools/ocamldep

#################
#make directories

DIRS=tools utils parsing typing driver bytecomp asmcomp toplevel compilerlibs \
     asmcomp/boot asmcomp/byte asmcomp/opt

$(addprefix boot_build/,$(DIRS)):
	mkdir -p $@

$(addprefix byte/,$(DIRS)):
	mkdir -p $@

$(addprefix opt/,$(DIRS)):
	mkdir -p $@

partialclean::
	rm -rf boot_build byte opt

#################

partialclean::
	for d in utils parsing typing bytecomp asmcomp driver toplevel tools; \
	  do rm -f $$d/*.cm[iox] $$d/*.annot $$d/*.[so] $$d/*~; done
	rm -f *~

depend: boot_depend byte_depend opt_depend

alldepend:: depend

#################

make_includes:
	$(TEMPLATER) Makefile.boot.var Makefile_variables.tmpl > Makefile_variables.boot
	$(TEMPLATER) Makefile.boot.var Makefile_rules.tmpl > Makefile_rules.boot
	$(TEMPLATER) Makefile.byte.var Makefile_variables.tmpl > Makefile_variables.byte
	$(TEMPLATER) Makefile.byte.var Makefile_rules.tmpl > Makefile_rules.byte
	$(TEMPLATER) Makefile.opt.var Makefile_variables.tmpl > Makefile_variables.opt
	$(TEMPLATER) Makefile.opt.var Makefile_rules.tmpl > Makefile_rules.opt

#################

distclean:
	$(MAKE) clean
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader boot/ocamlyacc \
	      boot/*.cm* boot/libcamlrun.a
	rm -f config/Makefile config/m.h config/s.h
	rm -f tools/*.bak tools/ocamlmklibconfig.ml
	rm -f ocaml ocamlc ocamlcomp.sh
	rm -f testsuite/_log

.PHONY: all backup bootstrap checkstack clean make_includes
.PHONY: partialclean beforedepend alldepend cleanboot coldstart
.PHONY: compare core coreall
.PHONY: boot_depend byte_depend opt_depend
.PHONY: coreboot defaultentry depend distclean install installopt
.PHONY: library library-cross libraryopt
.PHONY: ocamlbuild.byte ocamlbuild.native ocamldebugger ocamldoc
.PHONY: ocamldoc.opt lex/ocamllex lex/ocamllex.opt ocamltools ocamltoolsopt
.PHONY: ocamltoolsopt.opt yacc/ocamlyacc opt-core opt.opt otherlibraries
.PHONY: otherlibrariesopt package-macosx promote promote-cross
.PHONY: restore runtime runtimeopt makeruntimeopt world world.opt

include .boot_depend
include .byte_depend
include .opt_depend

#depend on directory
.SECONDEXPANSION:
$(sort $(ALL:.cmo=.cmx) $(ALL) $(ALL:.cmo=.cmi)): | $$(@D)
