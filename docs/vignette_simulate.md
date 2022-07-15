Welcome to the `simulate` package! This `R` package helps `R` coders to
conduct statistical simulations using their regression models.

## Why `simulate`?

*Statistical simulation* refers to generating model predictions for a
hypothetical observation with specific traits (usual the average case in
the dataset), using repeated simulations (random draws) to account for
estimation and fundamental uncertainty.

The idea was popularized by Gary King and colleagues’ Stata package
`Clarify` (King, Tomz, and Wittenberg 2000), and extended to `R` through
the work of Kosuke Imai, Olivia Lau, Gary King, Christine Choirat, and
many more via the `Zelig` package in `R`. Choirat et al. (2018).

With Zelig recently going offline, the `simulate` package provides a
lightweight stand-in for regular `Zelig` users, supporting basic models
from the `lm()`, `glm()`, `MASS::glm.nb()`, and `betareg:betareg()`
functions.

## Install Package from Github

You can install `simulate` from Github using the `devtools` package’s
`install_github()` function. (Sorry, no CRAN package right now!)

To get started, please load the `simulate` and `tidyverse` packages!
`simulate` is intended to be used in tandem with `tidyverse` functions
from `ggplot2` and `dplyr`; they are necessary for running `simulate`
functions.

## Workflow

Using the `simulate` package involves a short string of key functions,
including `equate()`, `calculate()`, `simulate()`, `tabulate()`, and
`illustrate()`. See Figure 1!

<div class="figure" style="text-align: left">

<div id="htmlwidget-f2ca511874fc5927918c" style="width:1000px;height:750px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-f2ca511874fc5927918c">{"x":{"diagram":"graph TD;\n  \n  %%% Nodes\n  m[model]\n  \n    e[\"equate()\"]\n    c[\"calculate(setx = list(...))\"]\n    s[\"simulate()\"]\n    g[\"group_by(case)\"]\n    t[\"tabulate()\"]\n    i[\"illustrate()\"]\n    \n\n  %% Start\n  m -->|Simulate Model Equations<br>from Multivariate Normal Distribution| e\n  \n  subgraph Estimation Error\n    e -->|Calculate Simulated Model Equations<br>given Conditions in setx| c\n  end\n\n  subgraph Fundamental Error\n    c -->|Simulate Predicted & Expected Values<br>from Monte Carlo Samples| s\n  end\n  \n  subgraph Reporting\n    s -->|for each condition| g\n    g -->|tabulate<br>quantities of interest| t\n    g -->|get illustration<br>data| i\n    e -->|tabulate<br>coefficient<br>table| t\n  end\n  \n  "},"evals":[],"jsHooks":[]}</script>
<p class="caption">
Figure 1: Sample Workflow in `simulate` package!
</p>

</div>

<br> <br>

## Example

For example, let’s use the famous `mtcars` dataset to simulate the
effects of horse power (`hp`) and cylinders (`cyl`) on the mileage
(`mpg`) of 32 fancy cars.

A usual `simulate` package query might look a bit like **this below**.

In the following sections, I’ll break down each of these functions with
examples!

<br> <br>

## 1. Make Model Object

First, we’ll use the `lm()` function to create a simple ordinary least
squares model, called `m`, with `mpg` as the outcome and `hp` and `cyl`
as predictors. We’ll turn `cyl` into a `factor()`, to demonstrate how to
use factors / fixed effects here.

*Note:* Current limitations of the package require that you turn
predictors into `factor()` format *before* regressing them in the model.
`simulate` will not work otherwise.

<br> <br>

## 2. Estimation Uncertainty with `equate()`

Next, we’ll use `equate()` to generate 1000 model equations, each very
close to the observed model equation in model `m`, but each just
slightly different. These slightly differing model equations represent
`estimation uncertainty`, the uncertainty in the true values of our beta
coefficients.

Exactly like in `Zelig` and `CLARIFY`, `equate()` takes random draws
from a multivariate normal distribution using the model’s coefficients
and variance-covariance matrix.

A bonus of the `simulate` package is that `equate()` gives you more
control, allowing you to work directly with our simulated equations, in
the event that you only want to account for estimation uncertainty, or
in the event that you want to reuse the exact same multivariate normal
distribution.

You can set a seed with `seed = 12345`, for example, to reproduce
results. To change the number of simulated equations sampled, adjust
`reps` from `reps = 1000` (default) to, for example, `reps = 3`. (Though
1000 is standard!)

    #> # A tibble: 3 × 5
    #>   replicate `(Intercept)`       hp  cyl6  cyl8
    #> *     <int>         <dbl>    <dbl> <dbl> <dbl>
    #> 1         1          28.2 -0.0310  -5.26 -6.91
    #> 2         2          30.1 -0.0384  -5.65 -7.01
    #> 3         3          26.4 -0.00544 -4.01 -9.03

<br> <br>

## 3. `calculate()` simulated values

Next, we’re going to use `calculate()` to compute our simulated
equations. By default, `calculate()` takes the median of numeric
variables and the mode of categorical variables, but you can simulate at
specific values, using the argument `setx = list(...)` (much like in
`Zelig`).

For example, let’s calculate the simulated `mpg` for an otherwise
average car as its `hp` increases from `100` to `150` to `200`.

    #> [1] "Predictor values set using contents of setx; otherwise, set to medians and modes by default."

`calculate` outputs a data.frame containing `replicate`, the id for each
simulated equation, `ysim`, the predictions from each simulated
equation, `sigma`, the residual standard error for the model, and
`case`, the unique ID for the *n* conditions supplied in `setx.` We used
`hp = c(100, 150, 200)`, totalling 3 conditions, so `case` runs from `1`
to `3`.

<br> <br>

## 4. `simulate()` fundamental uncertainty

Fourth, we can use `simulate()` to simulate fundamental uncertainty.
This takes each simulated value outputted by `calculate()` and then
constructs a distribution fitting the outcome variable (normal for
`lm()`, poisson for `glm(family = "Poisson")`, etc.)

`simulate()` outputs a `tibble` containing `replicate`, the unique ID
for each simulated equation, `pv`, the predicted value for that
equation, `ev`, the expected value for that equation, and `case`, the
unique ID for each condition specified in `setx` in the `calculate()`
function.

Check it out!

    #> # A tibble: 6 × 4
    #>   replicate  case    pv    ev
    #>   <chr>     <int> <dbl> <dbl>
    #> 1 1             1 21.8   20.0
    #> 2 1             2 20.1   17.9
    #> 3 1             3 15.5   15.7
    #> 4 10            1 13.8   15.3
    #> 5 10            2 16.4   14.4
    #> 6 10            3  7.90  13.6

But what *are* `ev` and `pv`?

-   To make predicted values (`pv`), it simulates 1 random draw from
    this distribution.

-   To make expected values (`ev`), it simulates 1000 random draws from
    this distribution, and takes the average of them.

Predicted values are encouraged when making predictions about the
future, since they generate wider confidence intervals, while expected
values are generally encouraged for explaining the past, as they
generate more precise estimates, averaging over fundamental uncertainty.

<br> <br>

We can do a lot with these values using `ggplot2`! The most direct
visualization is usually a `geom_point()` or `geom_jitter()`, but
`geom_violin()` can be handy at showing distributions too!

<img src="/tmp/Rtmptji9mE/preview-28f034778ec9.dir/simulate_files/figure-markdown_github/unnamed-chunk-11-1.png" width="100%" />

<br> <br>

## 5. `illustrate()` results

What if we want to visualize not just the overall range, but varying
confidence intervals? To get that data, we can use `illustrate()` on a
quantity of interest (`qi`). Be sure to use `group_by(case)` to get
confidence intervals for each different condition from `setx`!

Check it out! Each row has a unique ID describing a band, and together,
this tidy dataframe can be used to visualize the full range of
confidence intervals in `ggplot2.`

    #> # A tibble: 6 × 5
    #> # Groups:   case [1]
    #>    case group ci    lower upper
    #>   <int> <int> <fct> <dbl> <dbl>
    #> 1     1     1 99.9%  11.9  12.5
    #> 2     1     2 99%    12.5  14.0
    #> 3     1     3 95%    14.0  14.6
    #> 4     1     4 90%    14.6  17.8
    #> 5     1     5 90%    17.8  21.0
    #> 6     1     6 95%    21.0  21.5

We can visualize `mybands` like so in `ggplot2` (but for ideas, check
out my other tutorials on [RPubs](https://rpubs.com/timothyfraser)!)

<img src="/tmp/Rtmptji9mE/preview-28f034778ec9.dir/simulate_files/figure-markdown_github/unnamed-chunk-14-1.png" width="100%" />

`illustrate()` can even be used on other outputs, like our model
equations from `equate()`.

    #> # A tibble: 3 × 4
    #>   group ci      lower   upper
    #>   <int> <fct>   <dbl>   <dbl>
    #> 1     1 99.9% -0.0709 -0.0625
    #> 2     2 99%   -0.0625 -0.0548
    #> 3     3 95%   -0.0548 -0.0507

## 6. `tabulate()` results

Finally, do you just want a nice model table? Can do! We can tabulate
these results like so, supplying the `tabulate()` function a column to
tabulate.

<br>

### 6.1 Tabulating Confidence Intervals

`tabulate()` will return an `estimate`, the median simulated value for
your quantity of interest (`qi`), `lower` and `upper` simulated
confidence intervals (defaulting at 95% as shown by `ci = 0.95`), and
`se`, showing the standard deviation of simulations. We can even pair it
with `group_by(case)` to get estimates for each level.

    #> # A tibble: 3 × 8
    #>    case term  estimate lower upper    se p_value stars
    #>   <int> <chr>    <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>
    #> 1     1 ev        17.8  14.0  21.5 1.90        0 ***  
    #> 2     2 ev        16.6  14.1  18.9 1.23        0 ***  
    #> 3     3 ev        15.3  13.7  16.9 0.809       0 ***

<br>

### 6.2 Hypothesis Testing with `mu`

Further, you can easily test *whatever hypothesis you want* using the
`mu` parameter above. `mu` sets the null hypothesis for the estimate.
The `p_value` shows a one-tailed test of what percentage of simulations
cross `mu` from the estimate. As in, if the estimate is positive and
`mu = 0`, what share of simulations are negative? Our p-value for the
null hypothesis `mu = 0` above is `0%`, but that’s not very surprising.

Instead, `tabulate()`’s `mu` parameter becomes handy when testing
hypotheses about our simulations against real world benchmarks. For
instance, did PTSD rates after a disaster exceed the average level in
the population? As X increases from 1 to 10, how often do we expect
those rates to exceed `mu` percent of residents?

Or in this case, the median mileage (`mpg`) in our dataset is `19.2`
miles per gallon. So a meaningful quantity of interest might be, at what
level of horsepower does a car’s mileage really begin to exceed average
levels?

    #> [1] 19.2

We can test this hypothesis by setting `mu` to `19.2`. We see below that
otherwise-average cars with a `hp` of `100` (`case == 1`) do not tend to
have `mpg` that are significantly different from the median. However, as
their `hp` increases (`case == 2` and `case == 3`), their `mpg` actually
becomes significantly *lower* that median levels!

    #> # A tibble: 3 × 8
    #>    case term  estimate lower upper    se p_value stars
    #>   <int> <chr>    <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>
    #> 1     1 ev        17.8  14.0  21.5 1.90    0.236 " "  
    #> 2     2 ev        16.6  14.1  18.9 1.23    0.014 "*"  
    #> 3     3 ev        15.3  13.7  16.9 0.809   0     "***"

<br>

### 6.3 First Differences with `tabulate()`

`mu` is especially useful when computing first differences (also known
as marginal effects), meaning the change in expected or predicted
values. Let’s give it a try!

Check it out!

    #> # A tibble: 3 × 5
    #>   replicate  ev_1  ev_2  ev_3    fd
    #>   <chr>     <dbl> <dbl> <dbl> <dbl>
    #> 1 1          20.0  17.9  15.7 -4.36
    #> 2 10         15.3  14.4  13.6 -1.70
    #> 3 100        17.4  16.9  16.4 -1.05

Or if you feel more comfortable mixing base R and tidyverse coding, try
using brackets in `summarize()`, which does the same thing:

    #> # A tibble: 3 × 4
    #>   replicate  ev_1  ev_3    fd
    #>   <chr>     <dbl> <dbl> <dbl>
    #> 1 1          20.0  15.7 -4.36
    #> 2 10         15.3  13.6 -1.70
    #> 3 100        17.4  16.4 -1.05

### 6.4 Coefficient Tables

We might also just want to make a simple coefficent table using
`tabulate()`! Many models today violate the assumption of independence
of observations, which is required to compute standard errors from a
variance-covariance matrix. However, we could just simulate these error
instead, by taking the standard deviation from our estimation
uncertainty estimates from `equate()`.

When applying `tabulate()` to an output from `equate()`, you do not need
to specify a quantity of interest (`qi`); `tabulate()` will build the
coefficient table for all variables in the model.

Here, `mu = 0` by default, which is the usual null hypothesis for beta
coefficients. (However, you can change `mu` to anything, allowing you to
do mini-linear hypothesis tests if you want!)

    #> # A tibble: 4 × 7
    #>   term        estimate lower_ci upper_ci     se p_value stars
    #>   <chr>          <dbl>    <dbl>    <dbl>  <dbl>   <dbl> <chr>
    #> 1 (Intercept)  28.7     25.4    31.8     1.62     0     ***  
    #> 2 hp           -0.0240  -0.0548  0.00687 0.0157   0.064 .    
    #> 3 cyl6         -5.97    -9.06   -2.80    1.61     0.001 ***  
    #> 4 cyl8         -8.52   -13.2    -3.67    2.32     0     ***

(Note: All p-values from these simulations are currently one-tailed,
reflecting what share of simulations are on the opposite side of `mu`
from the estimate. However, this also means that we know that the
*other* tail *does* exceed *mu*, so two-tailed testing isn’t strictly
necessary for this method. Please report results accordingly!)

<br> <br>

# 7. Next Steps

Hope you enjoy! If you like it, share it! If you use the package, please
do cite it! Have questions? Feel free to reach out on
[Github](https://github.com/timothyfraser/simulate). Happy simulating!

    #> 
    #> To cite package 'simulate' in publications use:
    #> 
    #>   Fraser T (2022). _simulate: Tidy Simulation for Statistical Models_.
    #>   R package version 0.0.0.9000.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {simulate: Tidy Simulation for Statistical Models},
    #>     author = {Timothy Fraser},
    #>     year = {2022},
    #>     note = {R package version 0.0.0.9000},
    #>   }

<br> <br>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-choirat_et_al_2018" class="csl-entry">

Choirat, Christine, James Honaker, Kosuke Imai, Gary King, and Olivia
Lau. 2018. *Zelig: Everyone’s Statistical Software*.
<http://zeligproject.org/>.

</div>

<div id="ref-imai_et_al_2008" class="csl-entry">

Imai, Kosuke, Gary King, and Olivia Lau. 2008. “Toward a Common
Framework for Statistical Analysis and Development.” *Journal of
Computational Graphics and Statistics* 17 (4): 892–913.
<http://j.mp/msE15c>.

</div>

<div id="ref-king_et_al_2000" class="csl-entry">

King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most
of Statistical Analyses: Improving Interpretation and Presentation.”
*American Journal of Political Science*, 347–61.

</div>

</div>
