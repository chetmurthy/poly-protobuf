
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
	./test1 tests/testdata/benchmark/benchmark.proto
	./test1 tests/testdata/bs/bs_unittest.proto
	./test1 tests/testdata/google_unittest/unittest_import.proto
	./test1 tests/testdata/google_unittest/unittest_original.proto
	./test1 tests/testdata/google_unittest/unittest.proto
	./test1 tests/testdata/integration-tests/test01.proto
	./test1 tests/testdata/integration-tests/test02.proto
#	./test1 tests/testdata/integration-tests/test03.proto
	./test1 tests/testdata/integration-tests/test04.proto
	./test1 tests/testdata/integration-tests/test05.proto
	./test1 tests/testdata/integration-tests/test06.proto
	./test1 tests/testdata/integration-tests/test07.proto
	./test1 tests/testdata/integration-tests/test08.proto
	./test1 tests/testdata/integration-tests/test08.proto
	./test1 tests/testdata/integration-tests/test10.proto
	./test1 tests/testdata/integration-tests/test11.proto
	./test1 tests/testdata/integration-tests/test12.proto
	./test1 tests/testdata/integration-tests/test13.proto
	./test1 tests/testdata/integration-tests/test14.proto
	./test1 tests/testdata/integration-tests/test15.proto
	./test1 tests/testdata/integration-tests/test16.proto
	./test1 tests/testdata/integration-tests/test17.proto
	./test1 tests/testdata/integration-tests/test18.proto
	./test1 tests/testdata/integration-tests/test18.proto
	./test1 tests/testdata/integration-tests/test20.proto
	./test1 tests/testdata/integration-tests/test21.proto
#	./test1 tests/testdata/integration-tests/test22.proto
	./test1 tests/testdata/integration-tests/test23.proto
	./test1 tests/testdata/integration-tests/test24.proto
	./test1 tests/testdata/integration-tests/test25.proto
#	./test1 tests/testdata/integration-tests/test26.proto
#	./test1 tests/testdata/integration-tests/test27.proto
#	./test1 tests/testdata/yojson/yojson_unittest.proto

clean:
	rm -f pb0 pb0.opt *.cm* *.o *.ppo.ml

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r clexer.ml pb0.ml >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
