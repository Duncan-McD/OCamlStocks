MODULES= scraper authors parser cashset stockdata
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS) -tag thread

scraperdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files scraperdemo.byte && ./scraperdemo.byte -runner sequential

cashsetdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files cashsetdemo.byte && ./cashsetdemo.byte -runner sequential

stockdatademo: 
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files stockdatademo.byte && ./stockdatademo.byte -runner sequential

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip ocamlstocks.zip  README.md *.csv *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit ocamlstocks.opam .gitignore demo_files demo_files/*.ml* demo_files/demo.sh LICENSE Makefile	

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,mechaml,csv \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,mechaml,csv \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private 

demo:
	chmod u+x ./demo_files/demo.sh
	./demo_files/demo.sh
