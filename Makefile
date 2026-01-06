.PHONY: rix
rix:
	rm -f rix rix.o rix.ll
	cargo run -- main.rix -o rix.o
	gcc -o rix rix.o

.PHONY: rule110
rule110:
	rm -f rule110 rule110.o
	cargo run -- rule110.rix
	cargo run -- rule110.rix --emit-llvm
	gcc -o rule110 rule110.o

run: rix
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
