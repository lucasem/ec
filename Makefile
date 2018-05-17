C = corebuild

src := $(wildcard src/**/*.ml)
variants = str list
mini = src/mini.native


default: str list ec

ec: str
	@$(RM) ec
	ln -s str ec

$(variants): $(src)
	$(C) -pkg yojson -pkg re2 -I src/ec -quiet src/main_$@.native
	cp `readlink main_$@.native` $@
	$(RM) main_$@.native

results: ec
	./results/mk_data.sh
	@echo "results produced in results/data.tsv and results/learning_curves.eps"

mini: $(src)
	$(C) -pkg yojson -I src/ec -quiet $(mini)
	@echo "running mini:"
	@./$(mini)
	@echo ""
	@$(RM) $(mini)

run: ec
	./ec flashfill.json | tee out.json

clean:
	$(C) -clean
