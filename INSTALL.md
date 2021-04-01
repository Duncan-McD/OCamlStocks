# Installation Instructions
```
make install
```
This will automatically walk you through installing the required dependencies.


If for some reason this fails, follow the steps below:
1. Install these required packages using your relevant package manager:
```
pv
openssl
libssl-dev
```

2. Run this command (assuming you have `opam` installed already):
```
opam install . --deps-only
```

3. Enjoy