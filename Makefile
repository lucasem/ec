C = corebuild

default: main

main:
	$(C) -pkg yojson -quiet main.native

run: main
	$(RM) log/*
	./main.native | tee log/output

clean:
	$(RM) log/*
	$(C) -clean
