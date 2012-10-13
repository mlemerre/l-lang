all: doc next_

# Note: the order of the doc is important.
doc-files = support/union_find.mli support/union_find.ml cps/cpsvar.mli cps/cpsvar.ml cps/cpsbase.ml llvm/cpsllvm.mli llvm/cpsllvm.ml 

doc: $(addsuffix .html,$(addprefix web/,$(doc-files)))

web/%.html: src/%
	mkdir -p web/$(dir $*)
	ocamlweb -p "\usepackage{hevea}\usepackage{url}" --no-index doc/ocamlwebhevea.tex src/$* --hevea-option "-I /usr/share/texmf/tex/latex/misc" > web/$*.tex
	cd web/$(dir $*) && hevea -I /usr/share/texmf/tex/latex/misc ocamlweb.sty $(notdir $*).tex
	rm -f web/$*.tex web/$*.haux

next_:
	cd next && make

clean: 
	rm -Rf web
	rm -Rf src/support/_build
	rm -Rf src/_build
