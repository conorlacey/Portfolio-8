Portfolio 8
================
Conor Lacey
2023-04-14

``` r
suppressWarnings(library(tidyverse))
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

### Introduction

In this portfolio I am now going to animate a spike and slab graph to
demonstrate how this graph updates with new information. This will be
very similar to my previous portfolio except now I’m working with two
priors (i.e., the spike and the slab).

### What does it look like?

It is a good idea to first look at what exactly we are trying to create.

![Spike-and-Slab Plot Example](%22SpikeSlabPlot.png%22)
