
.PHONY: clean doc all

all: Main.hs Dnf2Bdd.hs
	ghc --make Main.hs -o formula-2-bdd
doc: Main.hs Dnf2Bdd.hs
	haddock -h -o doc/haddock  Main.hs Dnf2Bdd.hs

clean:
	rm -fr doc/haddock	

