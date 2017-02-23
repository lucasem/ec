C = corebuild

src := $(wildcard ./*.ml)
entry = main.native


default: ec

ec: $(src)
	$(C) -pkg yojson -quiet $(entry)
	mv $(entry) ec

run: ec
	./ec flashfill.json | tee out.json

clean:
	$(C) -clean
