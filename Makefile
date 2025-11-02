run: build
	./rix

build:
	cargo run
	gcc -o rix rix.o

dump:
	cargo run
	readelf -a rix.o
