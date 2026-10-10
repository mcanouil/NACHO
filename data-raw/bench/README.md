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

For comparison, NACHO 2.0.7, installed from the `v2.0.7` tag into a temporary library and run on the same `gen.R` data (`n = 768`, `seed = 1`), took about 9 s for the 768-sample run, recorded on 2026-09-29 on the same Apple M1 Pro running macOS Golden Gate 27.0 with R 4.6.1.
The machine load was not recorded for that run, so treat the comparison as approximate.

## Plot timings

`plots.R` times `autoplot()` and the drawing of the relative log expression (`RLE`), normalisation (`NORM`) and positive against negative (`PN`) plots on 48, 192 and 768 samples, into a 1200 by 700 pixel PNG with ragg.
Run it from the repository root with `Rscript data-raw/bench/plots.R`.
The `ragg` package is needed for this script only and is not a dependency of NACHO.

The target at 768 samples is under 1 s for each plot.

Recorded on 2026-10-09, on an Apple M1 Pro running macOS with R 4.6.1 and ggplot2 4.0.3.
The machine had a load average of about 5 from other work, so the times are on the pessimistic side.

```text
                  before    after
n =  48: RLE      0.76 s    0.37 s
n =  48: NORM     1.66 s    0.49 s
n =  48: PN       0.57 s    0.34 s
n = 192: RLE      1.72 s    0.36 s
n = 192: NORM     0.54 s    0.34 s
n = 192: PN       0.42 s    0.38 s
n = 768: RLE      5.61 s    0.63 s
n = 768: NORM     3.97 s    0.47 s
n = 768: PN       2.42 s    0.48 s
```

At 768 samples, RLE, NORM and PN each draw in under 1 s, which meets the target.

The relative log expression plot draws one line range and one crossbar layer from precomputed box statistics.
`geom_boxplot()` builds one grob per box, which took about 4 s for 768 boxes, while the crossbar layer draws all boxes in one pass.
