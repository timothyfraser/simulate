#' tabulate() Function
#'
#' This wrapper function computes various summary tables for statistical simulations.
#' @param data (Required) Data.frame containing output from [equations()] or [simulate()].
#'
#' @section Using [tabulate()] after [equations()]:
#' In this case, [tabulate()] will call this [@get_coeftable()] function, which draws on the following additional parameter.
#' @param ci (Optional) Single value from 0 to 1. Defaults to 0.95, for a 95% confidence interval.
#'
#' @section Using [tabulate()] after [simulate()]:
#' In this case, [tabulate()] will call the [get_quantiles()] function, which draws on the following additional parameters.
#' @param qi (Required) Name of vector (length 1) in data frame to summarize, written as a character vector. Typically ev, pv, or fd, but users can supply their own if needed.
#' @param ci (Optional) Single value from 0 to 1. Defaults to 0.95, for a 95% confidence interval.
#' @param mu (Optional) To compute statistical significance, specify the null hypothesis (eg. first difference = 0). mu = 0 is most common.
#' @param two.tailed (Optional) Defaults to TRUE. Logical indicating whether to use two-tailed (TRUE) or one-tailed (FALSE) tests of statistical significance. Two-tailed recommended.
#' @keywords coefficients
#' @export
#' @examples
#' 
#' # Load Packages
#' library(simulate)
#' library(tidyverse)
#' 
#' # Make a regression model
#' mymodel <- mtcars %>% lm(formula = mpg ~ disp + factor(cyl))
#' 
#' # Generate simulated coefficient table
#' mymodel %>%
#'   equations() %>%
#'   tabulate()
#' 
#' # To use a specific multivariate normal distribution,
#' # generate it first using equations(),
#' e <- mymodel %>% equations()
#' 
#' # then plug it into the tabulate() function
#' e %>% tabulate()
#' 
#' 

tabulate = function(data, qi = NULL, ci = 0.95, mu = NULL, two.tailed = TRUE){
  
  require(dplyr)
  require(tibble)
  # If a valid attribute is here,
  if(!is.null(attr(data, "step"))){
    # If the data comes from the equations function, 
    # it will have a step attribute == equations
    if(attr(data, "step") == "equations"){
      # so generate a coefficient table from it.
      output <- data %>% get_coeftable(., ci = ci)
    }else{
      # Otherwise, use get_quantiles
      output <- data %>% get_quantiles(., ci = ci, qi = qi, mu = mu, two.tailed = two.tailed)
    }
  }else{
    # If no valid attribute, still use get_quantiles
    output <- data %>% get_quantiles(., ci = ci, qi = qi, mu = mu, two.tailed = two.tailed)
  }
  
  output %>% return()
}


