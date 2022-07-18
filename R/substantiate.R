#' substantiate() Function
#'
#' This function compares simulated estimates from 2 models, to determine whether shared coefficients changed significantly between model specifications.
#' @param list_equations (Required) a list of 2 'equation' objects outputted from [equate()]. Should be from two different models. 
#' @param stat statistic to be calculated (either `"rmse"` or `"mpse"`). `"rmse"` returns root mean squared error (sigma) showing average difference between coefficients. `"mpse"` returns the square of `"rmse"`.
#' @param per level of tabulation (either `"model"` or `"terms"`). `"model"` returns statistic for change between all coefficients; `"terms"` returns statistic for change between each individual coefficient.
#' @param terms (Optional) character vector of coefficient names to include. (Defaults to all.)
#' @param drop_terms (Optional) character vector of coefficient names to exclude. (Defaults to none.)
#' @param drop_pattern (Optional) single character regex pattern of coefficient names to exclude. (Defaults to none.)
#' @param ci (numeric value) level of confidence interval (eg. 0.90, 0.95, 0.99, 0.999) (Defaults to `0.95`.) Used to [tabulate()] summary statistics.
#' @param mu (numeric value) alternative hypothesis for `stat`. (Defaults to `0`.) Used to [tabulate()] summary statistics.  
#' @keywords simulate
#' @export
#' @examples
#' 
#' # Load Packages
#' library(simulate)
#' library(tidyverse)
#' 
#'
#'# Let's use dplyr's delightful starwars dataset
#'data(starwars)
#'
#'# Let's predict the height of characters based on their species (eg. Ewok?)
#'m1 <- starwars %>%
#'  lm(formula = height ~ species)
#'# Let's predict the height of characters based on their species and gender
#'m2 <- starwars %>%
#'  lm(formula = height ~ species + gender)
#'
#'# Let's simulate 1000 model equations for each,
#'# and bind them into a list together.
#'mymodels <- list(
#'  m1 %>% equate(),
#'  m2 %>% equate())
#'
#'# Get RMSE (average difference in coefficients' standardized test statistics)
#'# at the overall model level
#'mymodels %>%
#'  substantiate(per = "model")
#'
#'# Get RMSE for each coefficeint
#'mymodels %>%
#'  substantiate(per = "terms")
#'
#'# Get RMSE for each coefficient,
#'# but compare only Gungans and Ewoks
#'mymodels %>%
#'  substantiate(per = "terms", term = c("speciesGungan", "speciesEwok"))
#'
#'# Get difference for each coefficient,
#'# but drop the intercept and Gungans
#'mymodels %>%
#'  substantiate(per = "terms", drop_terms = c("(Intercept)", "speciesGungan"))
#'
#'# Get the difference for shared coefficients,
#'# but exclude coefficients that contain speciesGungan or speciesEwok
#'mymodels %>%
#'  substantiate(per = "model", drop_pattern = "Gungan|Ewok")

substantiate = function(list_equations, stat = "rmse", per = "model", terms = NULL, drop_terms = NULL, drop_pattern = NULL, ci = 0.95, mu = 0){
  
  # Check length of list
  neq <- list_equations %>% length()
  
  # Add a friendly warning
  if(neq > 2){
    print("Warning: Only first 2 models in list will be compared. Please supply list with just 2 models to remove this warning.")
  }
  
  # Grab first two
  list_equations <- list(list_equations[[1]], list_equations[[2]])
  # Override any names present and set the names to be 1 and 2
  names(list_equations) <- c(1:2)
  
  # Take list
  mydist <- list_equations %>%
    # Bind together into tidy dataframe, with ids from model
    bind_rows(.id = "model") %>%
    # Pivot longer..., creating 'term' and 'sim' columns
    pivot_longer(cols = -c(model, replicate), names_to = "term", values_to = "sim",
                 values_drop_na = TRUE) %>%
    # Pivot wider..., creating 2 columns of values, m1 and m2
    pivot_wider(id_cols = c(replicate, term), names_from = model, values_from = sim,
                names_prefix = "m") %>%
    arrange(term, replicate) %>%
    # We can only compare variables that are shared between both models,
    # so we drop variables whose simulations are NOT present in either model
    filter(!is.na(m1) & !is.na(m2))
  
  # If they listed any drop_pattern, (eg. is NOT null),
  # then drop any variables that fit this regex pattern 
  # this might be a good idea for panel data,
  # where you might be interested in the coefficients for key independent variables,
  # but not annual fixed effects, which might all start with "year"
  if(!is.null(drop_pattern)){
    mydist <- mydist %>%
      filter(str_detect(term, pattern = drop_pattern, negate = TRUE)) 
  }
  
  # If they listed any drop_terms, (eg. is NOT null),
  # filter out these variable names specifically
  if(!is.null(drop_terms)){
    mydist <- mydist %>%
      filter(!term %in% drop_terms)
  }
  
  # If they listed any terms, (eg. is NOT null)
  # filter JUST TO THESE variable names specifically
  if(!is.null(terms)){
    mydist <- mydist %>%
      filter(term %in% terms)
  }
  
  
  # Standardize each variable's simulations
  mydist <- mydist %>%
    # For each variable,
    group_by(term) %>%
    # transform each coefficient into a standardized Z-statistic
    # by dividing by the standard deviation of simulations
    # (AKA standard error)
    # note: don't need to adjust for sample size, etc.
    # because these simulations all were generated 
    # from the variance-covariance matrix,
    # which already took that into account
    mutate(m1 = m1 / sd(m1),
           m2 = m2 / sd(m2)) %>%
    ungroup()
  
  
  if(!stat %in% c("rmse", "mpse")){
    print("Warning: 'stat' value entered is not currently supported. Only 'rmse' and 'mpse' supported. Defaulting to 'rmse'.")
  }
  
  if(!per %in% c("model", "terms")){
    print("Warning: 'per' value entered is not currently supported. Only 'model' and 'term' supported. Defaulting to 'model'.")
    
  }
  
  if(per == "model"){
    # Now calculate for EACH replicate,
    # how much did ALL variables as a whole change?
    mystat <- mydist %>%
      group_by(replicate) %>%
      summarize(mpse = sum((m2 - m1)^2) / (n() - 1),
                rmse = sqrt(mpse)) %>%
      ungroup() %>%
      # Tabulate p-values, assuming a null hypothesis of ZERO change
      tabulate(qi = stat, ci = ci, mu = mu)
    
  }else if(per == "terms"){
    # Now calculate for EACH variable,
    # how much did THAT variable's replicates change?
    mystat <- mydist %>%
      group_by(term) %>%
      # Get the root-squared error for each replicate
      # (can't take the mean, because each is just 1)
      # and abs() is the same as root-square sqrt( (.)^2)
      # Since we do it for each replicate,
      # this will form a distribution of normalized residuals
      mutate(residual = abs(m2 - m1)) %>%
      # Now for each variable, using the replicates
      group_by(term) %>%
      # Tabulate p-values, assuming a null hypothesis of ZERO change
      tabulate(qi = "residual", ci = ci, mu = mu) %>%
      ungroup() %>%
      # Add in a name
      bind_cols(tibble(type = "residual"), .)
  }

  # Now return!
  mystat %>%
    return() 
}



