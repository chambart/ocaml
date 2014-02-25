
boot_build/%.cmi: %.mli
	$(BOOT_CAMLC) $(BOOT_COMPFLAGS) -o $@ -c $<

boot_build/%.cmo: %.ml
	$(BOOT_CAMLC) $(BOOT_COMPFLAGS) -o $@ -c $<

boot_build/%.cmx: %.ml
	$(BOOT_CAMLOPT) $(BOOT_COMPFLAGS) -o $@ -c $<


$(BOOT_ALL) $(BOOT_ALL:.cmo=.cmi): stdlib/boot/stdlib.cma
$(BOOT_ALL:.cmo=.cmx): stdlib/boot/stdlib.cmxa

boot_build/compilerlibs/ocamlcommon.cma: $(BOOT_COMMON) | boot_build/compilerlibs
	$(BOOT_CAMLC) -a -o $@ $(BOOT_COMMON)

boot_build/compilerlibs/ocamlcommon.cmxa: $(BOOT_COMMON:.cmo=.cmx) | boot_build/compilerlibs
	$(BOOT_CAMLOPT) -a -o $@ $(BOOT_COMMON:.cmo=.cmx)


boot_build/compilerlibs/ocamlbytecomp.cma: $(BOOT_BYTECOMP) | boot_build/compilerlibs
	$(BOOT_CAMLC) -a -o $@ $(BOOT_BYTECOMP)

boot_build/compilerlibs/ocamlbytecomp.cmxa: $(BOOT_BYTECOMP:.cmo=.cmx) | boot_build/compilerlibs
	$(BOOT_CAMLOPT) -a -o $@ $(BOOT_BYTECOMP:.cmo=.cmx)


boot_build/compilerlibs/ocamltoplevel.cma: $(BOOT_TOPLEVEL) | boot_build/compilerlibs
	$(BOOT_CAMLC) -a -o $@ $(BOOT_TOPLEVEL)

boot_build/compilerlibs/ocamltoplevel.cmxa: $(BOOT_TOPLEVEL:.cmo=.cmx) | boot_build/compilerlibs
	$(BOOT_CAMLOPT) -a -o $@ $(BOOT_TOPLEVEL:.cmo=.cmx)


boot_build/compilerlibs/ocamloptcomp.cma: $(BOOT_ASMCOMP) | boot_build/compilerlibs
	$(BOOT_CAMLC) -a -o $@ $(BOOT_ASMCOMP)

boot_build/compilerlibs/ocamloptcomp.cmxa: $(BOOT_ASMCOMP:.cmo=.cmx) | boot_build/compilerlibs
	$(BOOT_CAMLOPT) -a -o $@ $(BOOT_ASMCOMP:.cmo=.cmx)


boot_build/ocamlc.opt: boot_build/compilerlibs/ocamlcommon.cmxa \
	         boot_build/compilerlibs/ocamlbytecomp.cmxa \
	         $(BOOT_BYTESTART:.cmo=.cmx) stdlib/boot/std_exit.cmx \
	         asmrun/libasmrun.a
	$(BOOT_CAMLOPT) $(LINKFLAGS) -o $@ \
	   boot_build/compilerlibs/ocamlcommon.cmxa \
	   boot_build/compilerlibs/ocamlbytecomp.cmxa \
	   $(BOOT_BYTESTART:.cmo=.cmx)

boot_build/ocamlopt.opt: boot_build/compilerlibs/ocamlcommon.cmxa \
	           boot_build/compilerlibs/ocamloptcomp.cmxa \
	           $(BOOT_OPTSTART:.cmo=.cmx) stdlib/boot/std_exit.cmx \
	           asmrun/libasmrun.a
	$(BOOT_CAMLOPT) $(LINKFLAGS) -o $@ \
	   boot_build/compilerlibs/ocamlcommon.cmxa \
	   boot_build/compilerlibs/ocamloptcomp.cmxa \
	   $(BOOT_OPTSTART:.cmo=.cmx)

boot_build/ocamlc: boot_build/compilerlibs/ocamlcommon.cma \
	           boot_build/compilerlibs/ocamlbytecomp.cma \
	           $(BOOT_BYTESTART) stdlib/boot/std_exit.cmo
	$(BOOT_CAMLC) $(LINKFLAGS) -compat-32 -o $@ \
	   boot_build/compilerlibs/ocamlcommon.cma \
	   boot_build/compilerlibs/ocamlbytecomp.cma \
	   $(BOOT_BYTESTART)

#FIXME: missing -compat-32
boot_build/ocamlopt: boot_build/compilerlibs/ocamlcommon.cma \
	             boot_build/compilerlibs/ocamloptcomp.cma \
	             $(BOOT_OPTSTART) stdlib/boot/std_exit.cmo
	$(BOOT_CAMLC) $(LINKFLAGS) -o $@ \
	   boot_build/compilerlibs/ocamlcommon.cma \
	   boot_build/compilerlibs/ocamloptcomp.cma \
	   $(BOOT_OPTSTART)


stdlib/boot/stdlib.cma: $(CAMLRUN)
	$(MAKE) -C stdlib boot/stdlib.cma

# Depends on stdlib.cma because .cmi are build using ocamlc maybe add a
# stdlib/all_cmi target that would also allow allow more parallelism
stdlib/boot/stdlib.cmxa: no_ocamlopt_for_boot \
                                      stdlib/boot/libasmrun.a \
                                      stdlib/boot/stdlib.cma
	$(MAKE) -C stdlib boot/stdlib.cmxa

stdlib/boot/std_exit.cmo: $(CAMLRUN)
	$(MAKE) -C stdlib boot/std_exit.cmo

stdlib/boot/std_exit.cmx: no_ocamlopt_for_boot
	$(MAKE) -C stdlib boot/std_exit.cmx

stdlib/boot/libasmrun.a: asmrun/libasmrun.a
	$(MAKE) -C stdlib boot
	cp asmrun/libasmrun.a stdlib/boot/libasmrun.a
