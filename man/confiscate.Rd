% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confiscate.R
\name{confiscate}
\alias{confiscate}
\title{confiscate() Function}
\usage{
confiscate(model, n = 5, cutoff = NULL)
}
\arguments{
\item{model}{(Required) Model object.}

\item{n}{(Optional) Number of highest outliers to cut, as measured by the highest Cook's distance scores.}

\item{cutoff}{(Optional) Numeric value stating score of Cook's distance \emph{after} which outliers will be cut.}
}
\description{
This function identifies and cuts \code{n} outliers from your model, then uses \code{\link[=formulate]{formulate()}} to return a data.frame that can be remodeled.
}
\examples{

# Load Packages
library(simulate)
library(tidyverse)

# Make a regression model
mymodel <- mtcars \%>\% lm(formula = mpg ~ disp + factor(cyl))

# Cut five highest outliers from the dataset
mymodel \%>\% 
 confiscate(n = 5) \%>\%
 lm()

# Alternatively, we could cut off any cases with a Cook's D higher than `cutoff`
# In our case, the highest cooks distance is about 0.15, 
# so we'll arbitrarily set the `cutoff` to 0.1 to demonstrate.
mymodel \%>\% 
 confiscate(cutoff = 0.1) \%>\%
 lm()
}
\keyword{outliers}
