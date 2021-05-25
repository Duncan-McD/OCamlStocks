# Installation Instructions

**Prerequisites:**
- Working opam, ocamlbuild, and ocaml environment
- Working python 2.7 installation (accessible in PATH with `python` command)

---

**First**:
```
pip install vaderSentiment
```

pip uses whatever version that is tied to the `python` command. 

so uses the version of pip tied to the python command

---

if this doesn't work you can install pip2 using the following instructions

```
curl https://bootstrap.pypa.io/pip/2.7/get-pip.py --output get-pip.py
sudo python2 get-pip.py
```


**Second**:
```
make install
```
This will automatically walk you through installing the required dependencies.


_If for some reason the second step fails, follow the steps below:_
1. Install these required packages using your relevant package manager:
```
pv
openssl
libssl-dev
libplplot-dev
libshp-dev
liblapacke-dev
libopenblas-dev
openblas
plplot
```

2. Run this command (assuming you have `opam` installed already if not install opam using your package installer):
```
opam install . --deps-only
```

3. Enjoy