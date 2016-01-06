all: treebench_mlton.exe treebench_strict_ghc.exe treebench_lazy_ghc.exe treebench_ocaml.exe

treebench_mlton.exe: treebench.sml
	time mlton -output $@ $^

treebench_strict_ghc.exe: treebench.hs
	time ghc -O2 $^ -o $@

treebench_lazy_ghc.exe: treebench_lazy.hs
	time ghc -O2 $^ -o $@

treebench_ocaml.exe: treebench.ml
	time ocamlopt.opt $^ -o $@

clean:
	rm -f *.exe *.o *.hi treebench treebench_lazy *.cmi *.cmo *.cmx
