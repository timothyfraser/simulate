#' simulate() Function
#'
#' This function accounts for fundamental uncertainty in model predictions, given a vector of 1 to N predicted values calculated by the calculate() function.
#' While the equations() and calculate() functions account for estimation uncertainty, by varying model coefficients, the simulate() function accounts for fundamental uncertainty by taking random draws from the appropriate distribution corresponding to the model used for predictions.
#' simulate() returns both predicted values (where each row represents 1 random draw) and expected values (where each row represents the mean of 1000 random draws)
#' @param data Data.frame of calculations outputted from the calculate() function. Required.
#' @param reps (Optional) Number of random draws to use for computing expected values. 1 ev = mean of N reps. Defaults to 1000.
#' @param seed (Optional) Seed for perfect replication of simulations. Defaults to seed specified in equations() unless otherwise specified.
#' @keywords simulate fundamental uncertainty
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
#' # Generate a multivariate normal distribution for a set of model coefficients.
#' mymodel %>%
#'   equations() %>%
#'   calculate(setx = list(disp = c(100, 200, 300))) %>%
#'   simulate()
#'

simulate = function(data, seed = NULL, reps = 1000){
  
  require(dplyr)
  require(stats)
  
  # If user supplies a specific seed, use it!
  # Otherwise, the seed for replication is always 12345.
  
  # If no seed is written, 
  # and a valid seed is available from the original model,
  if(is.null(seed) & !is.null(attr(data, "seed")) ){
    # use the same seed as in the original models.
    seed <- attr(data, "seed")
  }else{
    # If not available from original model,
    if(is.null(seed) & is.null(attr(data, "seed"))){
      # default to 12345
      seed <- 12345
    }
    # Otherwise, use the seed written
  }
  set.seed(seed)      
  
  # For all 1000 simulations AND each row of newdata submitted, run the following...
  
  type <- attr(data, "type")
  
  if(type == "lm"){
    output <- data %>%
      group_by(replicate, case) %>%
      summarize(pv = rnorm(n = 1, mean = ysim, sd = sigma),
                ev = rnorm(n = reps, mean = ysim, sd = sigma) %>% mean())
  }
  
  if(type == "logit"){
    output <- data %>%
      group_by(replicate, case) %>%
      summarize(pv = rbinom(n = 1, size = 1, prob = ysim),
                ev = rbinom(n = reps, size = 1, prob = ysim) %>% mean())
  }
  
  if(type == "poisson"){
    output <- data %>%
      group_by(replicate, case) %>%
      summarize(pv = rpois(n = 1, lamda = ysim),
                ev = rpois(n = reps, lamda = yhat) %>% mean())
  }
  if(type == "gamma"){
    output <- data %>%
      group_by(replicate, case) %>%
      summarize(pv = rgamma(n = 1, shape = ysim, scale = 1 / dispersion),
                ev = rgamma(n = reps, shape = ysim, scale = 1 / dispersion) %>% mean())
  }
  if(type == "negbin"){
    output <- data %>%
      group_by(replicate, case) %>%
      summarize(pv = rnegbin(n = 1, mu = ysim, theta = theta),
                ev = rnegbin(n = reps, mu = ysim, theta = theta) %>% mean())
  }
  if(type == "betareg"){
    output <- data %>%
      group_by(replicate, case) %>%
      summarize(pv = rbeta(n = 1, shape1 = ysim * phi, shape2 = (1 - ysim)*phi),
                ev = rbeta(n = reps, shape1 = ysim * phi, shape2 = (1 - ysim)*phi) %>% mean())
  }
  
  structure(.Data = output %>% ungroup(),
            step = "simulate",
            reps = reps,
            seed = seed) %>%
    return()
}
