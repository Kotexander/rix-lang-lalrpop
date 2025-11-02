run: build
	./rix

build:
	cargo run -- main.rix -o rix.o
	gcc -o rix rix.o

ir:
	cargo run -- main.rix --emit-llvm -o rix.ll

dump:
	cargo run -- main.rix -o rix.o
	objdump -d -Mintel rix.o

readelf:
	cargo run -- main.rix -o rix.o
	readelf -a rix.o
