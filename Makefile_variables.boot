
BOOT_UTILS= \
  boot_build/utils/misc.cmo boot_build/utils/tbl.cmo \
  boot_build/utils/config.cmo boot_build/utils/clflags.cmo \
  boot_build/utils/terminfo.cmo boot_build/utils/ccomp.cmo \
  boot_build/utils/warnings.cmo boot_build/utils/consistbl.cmo

BOOT_PARSING= \
  boot_build/parsing/location.cmo boot_build/parsing/longident.cmo \
  boot_build/parsing/ast_helper.cmo boot_build/parsing/syntaxerr.cmo \
  boot_build/parsing/parser.cmo boot_build/parsing/lexer.cmo \
  boot_build/parsing/parse.cmo boot_build/parsing/printast.cmo \
  boot_build/parsing/pprintast.cmo boot_build/parsing/ast_mapper.cmo

BOOT_TYPING= \
  boot_build/typing/ident.cmo boot_build/typing/path.cmo \
  boot_build/typing/primitive.cmo boot_build/typing/types.cmo \
  boot_build/typing/btype.cmo boot_build/typing/oprint.cmo \
  boot_build/typing/subst.cmo boot_build/typing/predef.cmo \
  boot_build/typing/datarepr.cmo boot_build/typing/cmi_format.cmo \
  boot_build/typing/env.cmo boot_build/typing/typedtree.cmo \
  boot_build/typing/printtyped.cmo boot_build/typing/ctype.cmo \
  boot_build/typing/printtyp.cmo boot_build/typing/includeclass.cmo \
  boot_build/typing/mtype.cmo boot_build/typing/envaux.cmo \
  boot_build/typing/includecore.cmo boot_build/typing/typedtreeIter.cmo \
  boot_build/typing/typedtreeMap.cmo boot_build/typing/cmt_format.cmo \
  boot_build/typing/includemod.cmo boot_build/typing/typetexp.cmo \
  boot_build/typing/parmatch.cmo boot_build/typing/stypes.cmo \
  boot_build/typing/typecore.cmo boot_build/typing/typedecl.cmo \
  boot_build/typing/typeclass.cmo boot_build/typing/typemod.cmo

BOOT_COMP= \
  boot_build/bytecomp/lambda.cmo boot_build/bytecomp/printlambda.cmo \
  boot_build/bytecomp/typeopt.cmo boot_build/bytecomp/switch.cmo \
  boot_build/bytecomp/matching.cmo \
  boot_build/bytecomp/translobj.cmo boot_build/bytecomp/translcore.cmo \
  boot_build/bytecomp/translclass.cmo boot_build/bytecomp/translmod.cmo \
  boot_build/bytecomp/simplif.cmo boot_build/bytecomp/runtimedef.cmo \
  boot_build/driver/pparse.cmo boot_build/driver/main_args.cmo \
  boot_build/driver/compenv.cmo boot_build/driver/compmisc.cmo

BOOT_COMMON=$(BOOT_UTILS) $(BOOT_PARSING) $(BOOT_TYPING) $(BOOT_COMP)

BOOT_BYTECOMP= \
  boot_build/bytecomp/meta.cmo boot_build/bytecomp/instruct.cmo \
  boot_build/bytecomp/bytegen.cmo boot_build/bytecomp/printinstr.cmo \
  boot_build/bytecomp/opcodes.cmo boot_build/bytecomp/emitcode.cmo \
  boot_build/bytecomp/bytesections.cmo boot_build/bytecomp/dll.cmo \
  boot_build/bytecomp/symtable.cmo boot_build/bytecomp/bytelink.cmo \
  boot_build/bytecomp/bytelibrarian.cmo boot_build/bytecomp/bytepackager.cmo \
  boot_build/driver/errors.cmo boot_build/driver/compile.cmo

BOOT_ASMCOMP= \
  boot_build/asmcomp/boot/arch.cmo boot_build/asmcomp/debuginfo.cmo \
  boot_build/asmcomp/cmm.cmo boot_build/asmcomp/printcmm.cmo \
  boot_build/asmcomp/reg.cmo boot_build/asmcomp/mach.cmo \
  boot_build/asmcomp/boot/proc.cmo boot_build/asmcomp/clambda.cmo \
  boot_build/asmcomp/printclambda.cmo boot_build/asmcomp/compilenv.cmo \
  boot_build/asmcomp/closure.cmo boot_build/asmcomp/cmmgen.cmo \
  boot_build/asmcomp/printmach.cmo boot_build/asmcomp/selectgen.cmo \
  boot_build/asmcomp/boot/selection.cmo \
  boot_build/asmcomp/comballoc.cmo boot_build/asmcomp/liveness.cmo \
  boot_build/asmcomp/spill.cmo boot_build/asmcomp/split.cmo \
  boot_build/asmcomp/interf.cmo boot_build/asmcomp/coloring.cmo \
  boot_build/asmcomp/reloadgen.cmo boot_build/asmcomp/boot/reload.cmo \
  boot_build/asmcomp/printlinear.cmo boot_build/asmcomp/linearize.cmo \
  boot_build/asmcomp/schedgen.cmo boot_build/asmcomp/boot/scheduling.cmo \
  boot_build/asmcomp/emitaux.cmo boot_build/asmcomp/emit.cmo \
  boot_build/asmcomp/asmgen.cmo boot_build/asmcomp/asmlink.cmo \
  boot_build/asmcomp/asmlibrarian.cmo boot_build/asmcomp/asmpackager.cmo \
  boot_build/driver/opterrors.cmo boot_build/driver/optcompile.cmo

BOOT_TOPLEVEL= \
  boot_build/toplevel/genprintval.cmo boot_build/toplevel/toploop.cmo \
  boot_build/toplevel/trace.cmo boot_build/toplevel/topdirs.cmo \
  boot_build/toplevel/topmain.cmo

BOOT_BYTESTART=boot_build/driver/main.cmo

BOOT_OPTSTART=boot_build/driver/optmain.cmo

BOOT_TOPLEVELSTART=boot_build/toplevel/topstart.cmo

BOOT_NATTOPOBJS=$(BOOT_UTILS) \
  $(BOOT_PARSING) $(BOOT_TYPING) \
  $(BOOT_COMP) $(BOOT_ASMCOMP) \
  boot_build/toplevel/genprintval.cmo boot_build/toplevel/opttoploop.cmo \
  boot_build/toplevel/opttopdirs.cmo boot_build/toplevel/opttopmain.cmo \
  boot_build/toplevel/opttopstart.cmo

BOOT_CVT_EMIT=boot_build/tools/cvt_emit.cmo
BOOT_MAKE_TEMPLATER=boot_build/tools/make_templater.cmo

BOOT_CAMLDEP_OBJ=boot_build/tools/depend.cmo boot_build/tools/ocamldep.cmo
BOOT_CAMLDEP_IMPORTS= \
  boot_build/utils/misc.cmo boot_build/utils/config.cmo \
  boot_build/utils/clflags.cmo boot_build/utils/terminfo.cmo \
  boot_build/utils/warnings.cmo boot_build/parsing/location.cmo \
  boot_build/parsing/longident.cmo boot_build/parsing/syntaxerr.cmo \
  boot_build/parsing/ast_helper.cmo boot_build/parsing/parser.cmo \
  boot_build/parsing/lexer.cmo boot_build/parsing/parse.cmo \
  boot_build/utils/ccomp.cmo boot_build/driver/pparse.cmo \
  boot_build/driver/compenv.cmo

BOOT_MKLIB=boot_build/tools/ocamlmklibconfig.cmo boot_build/tools/ocamlmklib.cmo

BOOT_ALL= \
         $(sort \
           $(BOOT_NATTOPOBJS) $(BOOT_TOPLEVELSTART) \
           $(BOOT_OPTSTART) $(BOOT_BYTESTART) \
           $(BOOT_TOPLEVEL) $(BOOT_ASMCOMP) \
           $(BOOT_BYTECOMP) $(BOOT_COMMON) \
           $(BOOT_CVT_EMIT) $(BOOT_CAMLDEP_OBJ) \
           $(BOOT_MAKE_TEMPLATER) $(BOOT_MKLIB))

BOOT_INCLUDES=-I boot_build/utils -I boot_build/parsing -I boot_build/typing \
	      -I boot_build/bytecomp -I boot_build/asmcomp -I boot_build/driver \
	      -I boot_build/toplevel -I boot_build/tools -I boot_build/asmcomp/boot
