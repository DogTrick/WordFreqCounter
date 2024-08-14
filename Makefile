GHC=ghc
BIN=word_freq_counter
FLAG=-o $(BIN) -O2

build: main.hs
	$(GHC) $(FLAG) main.hs

test: $(BIN)
	cat ./test.txt | ./$(BIN)

clean:
	rm *.hi *.o $(BIN)
