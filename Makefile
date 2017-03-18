C = corebuild

src := $(wildcard ./*.ml)
entry = main.native
mini = mini.native


default: ec

ec: $(src)
	$(C) -pkg yojson -quiet $(entry)
	cp `readlink $(entry)` ec
	$(RM) $(entry)

mini: $(src)
	$(C) -pkg yojson -quiet $(mini)
	@echo "running mini:"
	@./$(mini)
	@echo ""
	@$(RM) $(mini)

run: ec
	./ec flashfill.json | tee out.json

clean:
	$(C) -clean
