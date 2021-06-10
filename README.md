README.Rmd
================
Nate Lewis
2021-06-09

Load libraries

``` r
library(nflfastR)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.1     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(qs)
```

    ## qs v0.24.1.

Initial play-by-play load

``` r
future::plan("multisession")
pbp <- nflfastR::load_pbp(2010:2020, qs = TRUE) %>% 
  progressr::with_progress()
```

Filter out dead plays

``` r
data <- pbp %>%
  dplyr::filter(
    !is.na(.data$down),
    .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
  ) %>%
  decode_player_ids()
```

    ## ✓ 08:47:59 | Decoding of player ids completed

Load weekly stats

``` r
raw_wkly <- calculate_player_stats(pbp, weekly = TRUE) %>% 
  progressr::with_progress()
```
