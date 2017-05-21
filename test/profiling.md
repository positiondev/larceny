# Profiling

Run `stack bench --profile` to run benchmarks and generate a `prof` file.

Pipe the `prof` file into `ghc-prof-flamgeraph`, then the flamegraph script, then into a file.

```
cat bench.prof | stack exec ghc-prof-flamegraph | test/flamegraph.pl > flamegraph.svg
```
