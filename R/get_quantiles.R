#' get_quantiles() Function
#'
#' This underlying function derives a summary table based on predicted or expected values simulated by the simulate() function..
#' @param data (Required) Data.frame containing N predicted and expected values
#' @param qi (Required) Name of vector (length 1) in data frame to summary, written as a character vector. Typically ev, pv, or fd, but users can supply their own if needed.
#' @param ci (Optional) Single value from 0 to 1. Defaults to 0.95, for a 95% confidence interval.
#' @param mu (Optional) To compute statistical significance, specify the null hypothesis (eg. first difference = 0). mu = 0 is most common.
#' @param two.tailed (Optional) Defaults to TRUE. Logical indicating whether to use two-tailed (TRUE) or one-tailed (FALSE) tests of statistical significance. Two-tailed recommended.
#' @keywords expected predicted simulation table summary
#' @export
#' @seealso [summary()], the wrapper function for summarizing all quantities of interest from this package, and [get_coeftable()], for regression coefficient estimates.
#' @format
#' \describe{
#'   \item{term}{name of variable for which quantities of interest were calculated, eg. ev or pv.}
#'   \item{estimate}{median quantity of interest}
#'   \item{lower}{lower confidence interval for quantities of interest, based on input ci. When ci = 0.95, defaults to 2.5th percentile.}
#'   \item{upper}{upper confidence interval for quantities of interest, based on input ci. When ci = 0.95, defaults to 97.5th percentile.}
#'   \item{se}{standard error, representing standard deviation of quantities of interest.}
#'   \item{p_value}{p-value calculated from simulations. Depicts false positive rate as the percentage of simulations on opposite side of mu (usually 0) from the median quantity of interest. If two.tailed == TRUE, this rate is accordingly multiplied by 2. p-values shown only if mu is specified. p-values are only interpretable when mu is meaningful, such as when calculating first differences where mu = 0.}
#'   \item{stars}{Stars show statistical significance (*** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10.)}
#' }
#' @examples
#' 
#' # Load Packages
#' library(simulate)
#'
#'

get_quantiles = function(data, qi, ci = 0.95, mu = NULL, two.tailed = TRUE){
  # Get name of variable
  term <- qi
  
  if(two.tailed == TRUE){ tail = 2 }else{ tail = 1 }
  # If the user write a null hypothesis  (eg. mu = 0),
  # calculate how extreme a difference exists between the estimate versus mu.
  if(!is.null(mu)){
    mybands <- data %>%
      rename(qi = qi) %>%
      summarize(
        term = term,
        estimate = quantile(qi, probs = 0.50),
        lower = quantile(qi, probs = (1 - ci) / 2),
        upper = quantile(qi, probs = ci + (1 - ci) / 2),
        se = sd(qi),
        # Calculate p-value. 
        # By default, two-tailed == TRUE, multiply by 'tail' parameter (which will then = 2)
        # If two-tailed == FALSE, 'tail' paramter becomes just == 1.
        p_value = tail*(1 - if_else(condition = estimate > mu, 
                                    true = sum(qi > mu), 
                                    false = sum(qi < mu)  ) / n()),
        stars = gtools::stars.pval(p_value))
  }else{
    # If the user does NOT write a null hypothesis (where mu == NULL)
    # Just get the bands
    mybands <- data %>%
      rename(qi = qi) %>%
      summarize(
        term = term,
        estimate = quantile(qi, probs = 0.50),
        lower = quantile(qi, probs = (1 - ci) / 2),
        upper = quantile(qi, probs = ci + (1 - ci) / 2),
        se = sd(qi))
  }
  return(mybands)
  
}

