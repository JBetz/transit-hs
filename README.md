# transit-haskell

Implementation of the [transit](https://github.com/cognitect/transit-format) data interchange format in Haskell. Currently only targets JSON using the [aeson](https://github.com/bos/aeson) library.

## Completeness

**Scalar Types**

|Type|JSON|JSON-Verbose|MessagePack|
|---|---|---|---|
|null|yes|||
|string|yes|||
|boolean|yes|||
|integer, signed 64 bit|yes|||
|floating pt decimal|yes|||
|bytes|yes|| |
|keyword|yes|| |
|symbol|yes|| |
|arbitrary precision decimal|yes|| |
|arbitrary precision integer|yes|| |
|point in time|yes|||
|uuid|yes|||
|uri|yes|||
|char|yes|||
|quoted value|yes|||
|special numbers|no|||
|scalar extension type|no||

**Composite Types**

|Type|JSON|JSON-Verbose|MessagePack|
|---|---|---|---|
|array|yes|||
|map|yes|||
|set|yes|||
|list|yes|||
|map w/ composite keys|no|||
|link|no|||
|composite extension type|no|||


## Benchmarks

Comparison of decoding speed using sample data from the [transit spec](https://github.com/cognitect/transit-format/blob/master/examples/0.8/example.json):

```
benchmarking JSON sample/Aeson.decode
time                 2.276 ms   (2.222 ms .. 2.327 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 2.300 ms   (2.276 ms .. 2.335 ms)
std dev              96.03 μs   (68.43 μs .. 158.7 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking JSON sample/Transit.decodeTransitJSON
time                 5.154 ms   (5.057 ms .. 5.263 ms)
                     0.996 R²   (0.993 R² .. 0.997 R²)
mean                 5.350 ms   (5.269 ms .. 5.483 ms)
std dev              331.3 μs   (237.3 μs .. 527.6 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking JSON-Verbose sample/Aeson.decode
time                 2.584 ms   (2.532 ms .. 2.643 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 2.642 ms   (2.611 ms .. 2.711 ms)
std dev              152.0 μs   (82.67 μs .. 280.4 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking JSON-Verbose sample/Transit.decodeTransitJSON
time                 6.807 ms   (6.619 ms .. 7.005 ms)
                     0.995 R²   (0.992 R² .. 0.997 R²)
mean                 7.119 ms   (6.911 ms .. 7.733 ms)
std dev              1.048 ms   (215.1 μs .. 1.796 ms)
variance introduced by outliers: 77% (severely inflated)
```

## Developing

Run `nix-shell --run "ghcid -c 'cabal repl'"` in project root.

## Todo
- finish JSON implementation
- add golden tests from spec
- add and improve property tests
- implement verbose JSON writer
- add parser combinators
- distinguish internal errors from user errors
- support extension types
- compare performance with streaming parser
- implement MessagePack reader and writer