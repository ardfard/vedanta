SRC_DIR = src/*

build: $SRC_DIR
	cabal build -j4

run: 
	./dist/build/vedanta/vedanta
