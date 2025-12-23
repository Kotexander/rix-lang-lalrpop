build:
	rm -f rix rix.o rix.ll
	cargo run -- main.rix -o rix.o
	gcc -o rix rix.o

run: build
	./rix

ir:
	cargo run -- main.rix --emit-llvm -o rix.ll

ir-opt:
	cargo run -- main.rix --emit-llvm -O -o rix.ll

dump:
	cargo run -- main.rix -o rix.o -O
	objdump -d -Mintel rix.o

readelf:
	cargo run -- main.rix -o rix.o
	readelf -a rix.o
