C = corebuild

src := $(wildcard ./*.ml)
entry = main.native
mini = mini.native


default: ec

ec: $(src)
	$(C) -pkg yojson -quiet $(entry)
	cp `readlink $(entry)` ec
	$(RM) $(entry)

results: ec
	./results/mk_data.sh
	@echo "results produced in results/data.tsv and results/learning_curves.eps"

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
