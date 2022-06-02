# simulate
Simple package for tidyverse-friendly statistical simulation.

The ```simulate``` R package lets users simulate quantities of interest from statistical models, accounting for estimation and fundamental uncertainty. It serves as an homage to CLARIFY and Zelig, which recently went offline for a bit on CRAN. The main goal is to provide a very barebones structure for tidy simulation that anyone can use for basic models (and can quickly adapt for new models).

The workflow consistes of a few simple functions, including ```equations()```, ```calculate()```, ```simulate()```, ```tabulate()```, and ```bands()```.
