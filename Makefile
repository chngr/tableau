all:
	ghc --make YoungDiagrams.hs
	./YoungDiagrams -w 1000 -h 1000 -o Test.svg

clean:
	rm *.hi
	rm *.o
	rm Test.svg
