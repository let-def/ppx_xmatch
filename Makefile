PACKAGE=ppx_xmatch
OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
FLAGS=-package ocaml-migrate-parsetree,ppx_tools_versioned.metaquot_405
TARGETS=ppx_xmatch ppx_xmatch.cmo ppx_xmatch.cmx ppx_xmatch.cmxs

all: build
	
clean:
	rm -f *.o *.cm* $(TARGETS)

build: $(TARGETS)

install: build
	ocamlfind install $(PACKAGE) META $(TARGETS)

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

%.cmo: %.ml
	$(OCAMLC) $(FLAGS) -c $^

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c $^

ppx_xmatch.cmxs: ppx_xmatch.cmx
	$(OCAMLOPT) -o $@ -shared $^

ppx_xmatch: ppx_xmatch.cmx standalone.ml
	$(OCAMLOPT) $(FLAGS) -o $@ -linkpkg $^
