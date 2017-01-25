C = corebuild

default: main

main:
	$(C) -quiet main.native

run: main
	./main.native | tee log/output

clean:
	$(C) -clean
