C = corebuild

default: main

main:
	$(C) -quiet main.native

run: main
	$(RM) log/*
	./main.native | tee log/output

clean:
	$(RM) log/*
	$(C) -clean
