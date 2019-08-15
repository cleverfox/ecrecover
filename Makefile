
UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
	artefact = libecrecover.so
endif
ifeq ($(UNAME), Darwin)
	artefact = libecrecover.dylib
endif


all: priv/ecrecover.so

priv/ecrecover.so: src/lib.rs parity-ethereum/
	cargo build --release
	cp target/release/$(artefact) priv/ecrecover.so

clean:
	rm -f src/ecrecover.so target/release/libecrecover.so

parity-ethereum/:
	git clone https://github.com/johnsnewby/parity-ethereum.git
