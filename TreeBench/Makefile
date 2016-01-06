

all: treebench_mlton.exe treebench_ghc.exe

treebench_mlton.exe: treebench.sml
	time mlton -output $@ $^

treebench_ghc.exe: treebench.hs
	time ghc -O2 $^ -o $@

clean:
	rm -f *.exe
