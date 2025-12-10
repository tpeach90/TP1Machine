# TP1 Assembler

Assembles files written in TP1 Assembly.

Compile:
```bash
cargo build
```

Usage:
```bash
./target/debug/tp1assembler < ../examples/fibonacci.tp1a
```

Example output:
```
Label: screenPointer: 0
Label: prevTerm: 1
Label: currTerm: 2
Label: loop: 20
Label: skip: 24
Result: [4, 250, 33, 0, 4, 0, 33, 248, 224, 33, 1, 6, 1, 49, 249, 224, 49, 2, 2, 24, 12, 1, 14, 2, 72, 49, 1, 33, 2, 14, 0, 39, 224, 97, 49, 0, 131, 20, 4, 248, 33, 0, 2, 20]
Size: 44 bytes
```
