MODULES= scraper authors parser cashset stockdata algorithm
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS) -tag thread

algorithmdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files algorithmdemo.byte && ./algorithmdemo.byte -runner sequential

connotationdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files connotationdemo.byte && ./connotationdemo.byte -runner sequential

historydemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files historydemo.byte && ./historydemo.byte -runner sequential

scraperdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files scraperdemo.byte && ./scraperdemo.byte -runner sequential

cashsetdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files cashsetdemo.byte && ./cashsetdemo.byte -runner sequential

stockdatademo: 
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files stockdatademo.byte && ./stockdatademo.byte -runner sequential

parserdemo:
	ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files parserdemo.byte && ./parserdemo.byte -runner sequential

test:
	$(OCAMLBUILD) -tag thread -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip src.zip  README.md INSTALL.md *.csv *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit ocamlstocks.opam testing_files testing_files/stocksnew.json .gitignore demo_files demo_files/*.ml* demo_files/demo.sh LICENSE Makefile	

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

install:
	chmod u+x system_dependencies.sh
	./system_dependencies.sh
	opam install . --deps-only

demo1:
	chmod u+x ./demo_files/demo.sh
	./demo_files/demo.sh

demo2:
	chmod u+x ./demo_files/demo2.sh
	./demo_files/demo2.sh

demo:
	make demo1
	sleep 5
	make demo2
