################################################################
# The build system now uses omake, rather than GNU Make +
# ocamlbuild.
omake:
	omake

# Below remains what has not yet been converted.
all: unit_tests # doc

################################################################
# Documentation.

# Note: the order of the doc is important.
# Note: it should be top-down: more important things first, details
# second. This presumes that what does a lower-level module is
# understandable just by looking at the code that use it.
doc-files = support/union_find.mli support/union_find.ml cps/cpsbase/cpsast.ml cps/cpsbase.mli cps/cpsbase.ml \
	cps/cpsbase/cpsvar.mli cps/cpsbase/cpsvar.ml cps/cpsbase/cpsdef.mli \
	cps/cpsbase/cpsdef.ml cps/cpsbase/cpsprint.ml cps/cpsbase/cpscheck.ml \
	cps/cpsconvertclosures.ml cps/cpsfree.ml \
	llvm/cpsllvm.mli llvm/cpsllvm.ml\
	support/unique.mli support/unique.ml\
	cps/cpstransform/cpstransform_rules.ml\
	compilation_passes.ml

doc: $(addsuffix .html,$(addprefix web/,$(doc-files))) check_doc web/cps/cpstransform/cpstransform_rules.ml001.png

web/cps/cpstransform/cpstransform_rules.ml001.png: doc/cpstransform_rules_example.tex
	cd doc && pdflatex cpstransform_rules_example.tex
	convert -trim -density 150 -transparent white doc/cpstransform_rules_example.pdf $@


web/%.html: src/%
	mkdir -p web/$(dir $*)
	ocamlweb -p "\usepackage{hevea}\usepackage{url}\usepackage{graphics}" --no-index doc/ocamlwebhevea.tex src/$* --hevea-option "-I /usr/share/texmf/tex/latex/misc" > web/$*.tex
	cd web/$(dir $*) && hevea -I /usr/share/texmf/tex/latex/misc ocamlweb.sty $(notdir $*).tex
	rm -f web/$*.tex web/$*.haux

next_:
	cd next && make


# If some files are not in $(doc-files), report an error.
check_doc: 
	@TMP1=`mktemp`; TMP2=`mktemp`; TMP3=`mktemp`;\
	find src -name "*_build*" -prune -o -name "*.ml*" -print | sort > $$TMP1 ;\
	for i in $(doc-files); do echo "src/$$i" >> $$TMP2; done; \
	sort $$TMP2 -o $$TMP2;\
	comm -23 $$TMP1 $$TMP2 > $$TMP3 ;\
	if [ "0" != "`wc -l $$TMP3`" ]; then \
	  echo "Error: The documentation for the following files is not generated:"; \
	  cat $$TMP3; \
	  false; \
	 fi


################################################################

clean: 
	rm -Rf web
	rm -Rf src/support/_build
	rm -Rf src/_build
	rm -Rf _build
	rm -f test_*.byte

.PHONY: unit_tests
