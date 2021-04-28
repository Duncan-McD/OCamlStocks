# Installation Instructions

**Prerequisites:**
- Working opam, ocamlbuild, and ocaml environment
- Working python installation (accessible in PATH with `python` command)

---

**First**:
```
pip install vaderSentiment
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
```

2. Run this command (assuming you have `opam` installed already if not install opam using your package installer):
```
opam install . --deps-only
```

3. Enjoy