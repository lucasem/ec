C = corebuild

OBJ := main.native

all: main

main:
	$(C) $(OBJ)

run: main
	./main.native | tee log/output

clean:
	-$(RM) $(OBJ)
	-$(RM) -r _build
