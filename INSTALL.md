# Installation Instructions

## Prerequisites:
- Working opam, ocamlbuild, and ocaml environment
- Working python 2.7 installation (accessible in PATH with `python` command)

---

## First:
```
pip install vaderSentiment
```

_You may need to install pip2 since MacOS and Ubuntu installations of Python 2 do not include pip2 by default. To install pip2 use the following commands_

```
curl https://bootstrap.pypa.io/pip/2.7/get-pip.py --output get-pip.py
sudo python get-pip.py
```


## Second:
```
make install
```
This will automatically walk you through installing the required dependencies.


_If for some reason the second step fails, follow the steps below:_
1. Install these required packages using your relevant package manager:
```
pv
openssl
libssl-dev  (not required for MacOS)
w3m
```

1. Run this command (assuming you have `opam` installed already):
```
opam install . --deps-only
```

3. Enjoy

---

## Troubleshooting:
A known error exists for MacOS users with versions of `openssl`. This may occur when initializing the project (i.e. running `make test` or `make demo`) and will appear immediately. The error will look very similar to the example error message below.

```
ocamlbuild -use-ocamlfind -tag 'debug' -tag thread -I demo_files historydemo.byte && ./historydemo.byte -runner sequential
+ ocamlfind ocamlc -linkpkg -g -thread -package ounit2 -package mechaml -package yojson -package csv -package pyml -I demo_files cashset.cmo scraper.cmo stockdata.cmo parser.cmo demo_files/historydemo.cmo -o demo_files/historydemo.byte
ld: warning: directory not found for option '-L/usr/local/Cellar/openssl@1.1/1.1.1j/lib'
ld: library not found for -lssl
clang: error: linker command failed with exit code 1 (use -v to see invocation)
File "_none_", line 1:
Error: Error while building custom runtime system
Command exited with code 2.
Compilation unsuccessful after building 21 targets (20 cached) in 00:00:01.
```

Your specific error may include an additional line as follows:
```
ld: warning: directory not found for option '-L/opt/local/lib'
```
and may also contain a different letter after `1.1.1` such as `1.1.1i`.

To fix this error you must copy the directory found in `/usr/local/Cellar/openssl@1.1/` which will be named `1.1.1_`, where `_` is a letter, and rename it as `1.1.1*`, where `*` is the letter after `1.1.1` that appears in the error message (i.e. `1.1.1j` in the example above).