# simulate
Simple package for tidyverse-friendly statistical simulation.

The ```simulate``` R package lets users simulate quantities of interest from statistical models, accounting for estimation and fundamental uncertainty. It serves as an homage to CLARIFY and Zelig, which recently went offline for a bit on CRAN. The main goal is to provide a very barebones structure for tidy simulation that anyone can use for basic models (and can quickly adapt for new models).

The workflow consistes of a few simple functions, including ```equate()```, ```calculate()```, ```simulate()```, ```tabulate()```, and ```illustrate()```.

## Installation

To install the latest development version from GitHub, use the `devtools` package's `install_github()` function!

```r
devtools::install_github("timothyfraser/simulate")
```

## Citation

To cite package `simulate` in publications, please use:

- Fraser T (2022). *simulate: Tidy Simulation for Statistical Models.* R package version 0.0.0.9000. https://github.com/timothyfraser/simulate


A BibTeX entry for LaTeX users is

```bibtex
  @Manual{,
    title = {simulate: Tidy Simulation for Statistical Models},
    author = {Timothy Fraser},
    year = {2022},
    note = {R package version 0.0.0.9000},
  }
```

To learn more, check out the vignette!
