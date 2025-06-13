GHC=ghc
BIN=word_freq_counter
FLAG=-o $(BIN) -O2

build: Main.hs
	$(GHC) -package containers $(FLAG) Main.hs

test: $(BIN)
	cat ./test.txt | ./$(BIN)

clean:
	rm *.hi *.o $(BIN)
