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
	cps/cpstransform/base.ml cps/cpstransform/expression.ml\
	cps/cpstransform/rules.ml cps/cpstransform/definition.ml \
	compilation_passes.ml \
	parser/tdop.mli parser/tdop.ml \
	parser/parser_type.ml # parser/parser_expression.ml

doc: $(addsuffix /index.html,$(addprefix web/,$(doc-files))) check_doc web/cps/cpstransform/cpstransform_rules.ml001.png
#	rm -Rf ~/Cloud/UbuntuOne/src-blessed/site/jekyll/documentation/compiler-book && mv web ~/Cloud/UbuntuOne/src-blessed/site/jekyll/documentation/compiler-book

jekyll: $(addsuffix /index.html,$(addprefix web4jekyll/,$(doc-files))) web/cps/cpstransform/cpstransform_rules.ml001.png # check_doc

web/parser/grammar.tex: src/parser/parser_type.ml # src/parser/parser_expression.ml src/parser/parser_definition.ml
	awk '/\\begin{grammar}/,/\\end{grammar}/' $^ \
	| sed 's/\\begin{grammar}//g' \
	| sed 's/\\end{grammar}//g' \
	| sed 's/(\*//g' \
	| sed 's/\*)//g' > $@

web/cps/cpstransform/cpstransform_rules.ml001.png: doc/cpstransform_rules_example.tex
	cd doc && pdflatex cpstransform_rules_example.tex
	convert -trim -density 150 -transparent white doc/cpstransform_rules_example.pdf $@


web/%/index.html: src/%
	mkdir -p web/$*
	ocamlweb -p "\usepackage{hevea}\usepackage{url}\usepackage{graphics}" --no-index doc/ocamlwebhevea.tex src/$* --hevea-option "-I /usr/share/texmf/tex/latex/misc" > web/$*/index.tex
	cd web/$* && hevea -I /usr/share/texmf/tex/latex/misc ocamlweb.sty index.tex
	rm -f web/$*/index.tex web/$*/index.haux

web4jekyll/%/index.html: web/%/index.html
#	This is the handling for the final publication using jekyll.
	mkdir -p web4jekyll/$*/
	cp web/$*/index.html web4jekyll/$*/index.tostrip.html
	echo -n "---\nlayout: compiler_book\nactive: doc\ntitle: $*\nmenu_entry: /documentation/compiler_hyperbook/$*\n---\n{% raw %}\n" > web4jekyll/$*/index.html
# Cut between CUT DEF and CUT END lines; change the non-breaking space
# from unicode to html.
	cat web4jekyll/$*/index.tostrip.html | sed -n '/CUT DEF/,/CUT END/p' | sed 's/\xA0/\&nbsp;/g' >> web4jekyll/$*/index.html
	echo -n "\n{% endraw %}\n" >> web4jekyll/$*/index.html

# Convert the file to utf-8; else jekyll complains.
	mv web4jekyll/$*/index.html web4jekyll/$*/index.html.8859
	iconv -f iso-8859-15 -t utf-8 web4jekyll/$*/index.html.8859 > web4jekyll/$*/index.html
	rm -f web4jekyll/$*/index.tostrip.html web4jekyll/$*/index.html.8859

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
