% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/illustrate.R
\name{illustrate}
\alias{illustrate}
\title{illustrate() Function}
\usage{
illustrate(data, qi, ci = c(0.9, 0.95, 0.99, 0.999))
}
\arguments{
\item{data}{(Required) Data.frame containing output from \code{\link[=simulate]{simulate()}}.
In this case, \code{\link[=illustrate]{illustrate()}} will query \code{\link[=tabulate]{tabulate()}} multiple times, using the \code{\link[=get_quantiles]{get_quantiles()}} function with the following additional parameters.}

\item{qi}{(Required) Name of vector (length 1) in data frame to summarize, written as a character vector. Typically ev, pv, or fd, but users can supply their own if needed.}

\item{ci}{(Optional) Single value from 0 to 1. Defaults to 0.95, for a 95\% confidence interval.}
}
\description{
This wrapper function tabulates many confidence intervals for quantities of interest at once, eg. 90, 95, 99, and 99.9\% intervals, for use in visualization. Intended for use after \code{\link[=simulate]{simulate()}}.
}
\examples{

' # Make a regression model
mymodel <- mtcars \%>\% lm(formula = mpg ~ disp + factor(cyl))

# Generate a multivariate normal distribution for a set of model coefficients.
mysim <- mymodel \%>\%
  equate() \%>\%
  calculate(setx = list(disp = c(100, 200, 300))) \%>\%
  simulate() 
  
# Let's get all the confidence intervals   
mysim \%>\%
  # For each condition added in setx above, 
  group_by(case) \%>\%
  # get all confidence intervals for expected values
  illustrate(qi = "ev")

# You can even use it for first differences.
mysim \%>\% 
  pivot_wider(id_cols = c(replicate), 
     names_from = case, 
     values_from = ev, 
     names_prefix = "ev") \%>\%
   # Calculate first differences
   mutate(fd = ev2 - ev1) \%>\%
   illustrate(qi = "fd")
 
 # Notice how it's not necessary to use group_by(case) here, 
 # because the data.frame no longer has cases? 
 # We pivoted the cases into separate columns,
 #  then subtracted the expected values 
 # for case 2 minus case 1 to produce first differences.

 # Finally, you can visualize these in ggplot like so:
 
mysim \%>\%
  group_by(case) \%>\%
  illustrate(qi = "ev") \%>\%
  ggplot(mapping = aes(x = case, y = estimate, ymin = lower, ymax = upper, fill = ci)) +
  geom_crossbar()
 
 
 # Happy simulating!
 
}
\keyword{coefficients}
