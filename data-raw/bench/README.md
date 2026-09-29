# `load_rcc()` benchmark

`bench.R` writes synthetic single-sample RCC files with `gen.R` (774 probes each) and times `load_rcc()` on 48, 192 and 768 samples.
Run it from the repository root with `Rscript data-raw/bench/bench.R`.

The targets at 768 samples are a `load_rcc()` call under 3 s and an object under 10 MB.

## Results

Recorded on 2026-09-28, on an Apple M1 Pro running macOS Golden Gate 27.0 with R 4.6.1 and the reference BLAS.
The machine had a load average of about 4.5 from other work, so the times are on the pessimistic side.

```text
n =   48: load_rcc 0.09 s, object 1.0 MB
n =  192: load_rcc 0.38 s, object 2.4 MB
n =  768: load_rcc 2.49 s, object 7.9 MB
targets at 768 samples: load_rcc under 3 s PASS, object under 10 MB PASS
```

For comparison, NACHO 2.0.7 took about 9 s for the same 768-sample run.
