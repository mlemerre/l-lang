all: unit_tests doc 

################################################################
# Unit tests.

test_union_find:
	ocamlbuild -I src/support unit_tests/support/test_union_find.byte && ./test_union_find.byte

unit_tests: test_union_find

################################################################
# Documentation.

# Note: the order of the doc is important.
doc-files = support/union_find.mli support/union_find.ml cps/cpsvar.mli cps/cpsvar.ml \
	cps/cpsbase.ml cps/cpsprint.mli cps/cpsprint.ml \
	llvm/cpsllvm.mli llvm/cpsllvm.ml

doc: $(addsuffix .html,$(addprefix web/,$(doc-files))) check_doc

web/%.html: src/%
	mkdir -p web/$(dir $*)
	ocamlweb -p "\usepackage{hevea}\usepackage{url}" --no-index doc/ocamlwebhevea.tex src/$* --hevea-option "-I /usr/share/texmf/tex/latex/misc" > web/$*.tex
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
