
TOP=.

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
PACKAGES=camlp5,fmt,camlp5.extprint,camlp5.extend,camlp5.lexer,camlp5.pprintf,pcre,pa_ppx.runtime

all: pb0 pb0.opt

pb0: clexer.cmo pb0.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -linkall -linkpkg -syntax camlp5r $^ -o $@

pb0.opt: clexer.cmx pb0.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -linkall -linkpkg -syntax camlp5r $^ -o $@

.SUFFIXES: .ml .cmo .cmx

pb0.cmo: pb0.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

pb0.cmx: pb0.ml
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

clexer.cmo: clexer.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

clexer.cmx: clexer.ml
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

test: pb0
	./test1 tests/testdata/foo.proto
	./test1 tests/testdata/benchmark/benchmark.proto
	./test1 tests/testdata/bs/bs_unittest.proto
	./test1 tests/testdata/google_unittest/unittest_import.proto
	./test1 tests/testdata/google_unittest/unittest_original.proto
	./test1 tests/testdata/google_unittest/unittest.proto

clean:
	rm -f pb0 pb0.opt *.cm* *.o *.ppo.ml

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r clexer.ml pb0.ml >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
