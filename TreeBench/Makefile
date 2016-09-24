all: treebench_mlton.exe treebench_strict_ghc.exe treebench_lazy_ghc.exe treebench_ocaml.exe \
     treebench_rust.exe treebench_rust_sys_alloc.exe

treebench_mlton.exe: treebench.sml
	time mlton -output $@ $^

treebench_strict_ghc.exe: treebench.hs
	time ghc -O2 -rtsopts $^ -o $@

treebench_lazy_ghc.exe: treebench_lazy.hs
	time ghc -O2 -rtsopts $^ -o $@

treebench_ocaml.exe: treebench.ml
	time ocamlopt.opt $^ -o $@

treebench_rust.exe: treebench.rs
	time rustc $^ -o $@ -O

treebench_rust_sys_alloc.exe: treebench_sys_alloc.rs
	time rustc $^ -o $@ -O

clean:
	rm -f *.exe *.o *.hi treebench treebench_lazy *.cmi *.cmo *.cmx
