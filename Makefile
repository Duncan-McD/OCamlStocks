MODULES= scraper authors parser cashset stockdata algorithm portfolio user grapher config state uniformtesting saveload auth
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
DEPS= yojson,mechaml,csv,mechaml,pyml,owl,owl-plplot,graphics,camlimages.graphics,camlimages.png,ANSITerminal

default: bot

utop: build

build:
	$(OCAMLBUILD) -I src $(OBJECTS) -tag thread

bot: 
	$(OCAMLBUILD) -I src -tag 'debug' -tag thread $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

test:
	$(OCAMLBUILD) -I testing_files -tag thread -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip src.zip  README.md INSTALL.md *.csv *.ml* *.json *.sh _tags .merlin .ocamlformat *.png .ocamlinit ocamlstocks.opam testing_files testing_files/*.json .gitignore LICENSE Makefile	

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(DEPS) \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(DEPS) \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private 

install:
	chmod u+x system_dependencies.sh
	./system_dependencies.sh
	opam install . --deps-only

lines:
	cloc --by-file --exclude-dir=demo_files,_build --include-lang=OCaml .