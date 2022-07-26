#' illustrate() Function
#'
#' This wrapper function tabulates many confidence intervals for quantities of interest at once, eg. 90, 95, 99, and 99.9% intervals, for use in visualization. Intended for use after [simulate()].
#' @param data (Required) Data.frame containing output from [simulate()].
#' In this case, [illustrate()] will query [tabulate()] multiple times, using the [get_quantiles()] function with the following additional parameters.
#' @param qi (Required) Name of vector (length 1) in data frame to summarize, written as a character vector. Typically ev, pv, or fd, but users can supply their own if needed.
#' @param ci (Optional) Single value from 0 to 1. Defaults to 0.95, for a 95% confidence interval.
#' @keywords coefficients
#' @export
#' @examples
#' 
#'' # Make a regression model
#' mymodel <- mtcars %>% lm(formula = mpg ~ disp + factor(cyl))
#' 
#' # Generate a multivariate normal distribution for a set of model coefficients.
#' mysim <- mymodel %>%
#'   equate() %>%
#'   calculate(setx = list(disp = c(100, 200, 300))) %>%
#'   simulate() 
#'   
#' # Let's get all the confidence intervals   
#' mysim %>%
#'   # For each condition added in setx above, 
#'   group_by(case) %>%
#'   # get all confidence intervals for expected values
#'   illustrate(qi = "ev")
#' 
#' # You can even use it for first differences.
#' mysim %>% 
#'   pivot_wider(id_cols = c(replicate), 
#'      names_from = case, 
#'      values_from = ev, 
#'      names_prefix = "ev") %>%
#'    # Calculate first differences
#'    mutate(fd = ev2 - ev1) %>%
#'    illustrate(qi = "fd")
#'  
#'  # Notice how it's not necessary to use group_by(case) here, 
#'  # because the data.frame no longer has cases? 
#'  # We pivoted the cases into separate columns,
#'  #  then subtracted the expected values 
#'  # for case 2 minus case 1 to produce first differences.
#'
#'  # Finally, you can visualize these in ggplot like so:
#'  
#' mysim %>%
#'   group_by(case) %>%
#'   illustrate(qi = "ev") %>%
#'   ggplot(mapping = aes(x = case, y = estimate, ymin = lower, ymax = upper, fill = ci)) +
#'   geom_crossbar()
#'  
#'  
#'  # Happy simulating!
#'  

illustrate = function(data, qi, ci = c(0.90, 0.95, 0.99, 0.999)){

  require(dplyr)
  require(tibble)
  require(gtools)
  require(purrr)
  require(simulate)
  
  # Write a simple base function to tabulate for a given dataset
  get_band = function(d, qi, ci){
    d %>%
      tabulate(qi = qi, ci = ci, 
               # Don't generate hypothesis tests for this portion.
               mu = NULL) %>%
      mutate(ci = ci) %>%
      return()
  }
  
  # If a valid attribute is here,
  #if(!is.null(attr(data, "step"))){
    # If the data comes from the equate function, 
    # it will have a step attribute == equations
  #  if(attr(data, "step") == "equations"){
      # so generate quantiles from it
      # For each level of confidence interval,
  #    output <- ci %>% 
        # using the get_quantiles function
  #      map_dfr(~get_quantiles(data = data, ci = ., qi = qi, mu = NULL))
      
  #}else{
  
  # For each level of confidence interval,
  output <- ci %>%
    # Run the get_band function
    map_dfr(~get_band(d = data, qi = qi, ci = .))
  
  
  # Unfortunately, map_dfr breaks any existing groups
  # So we're going to have re-instate those groups...
  
  # If the original dataframe was grouped, 
  # we're going to reinstate those groups now
  if(is_grouped_df(data)){
    # Grab all grouping variable names
    mygroups <- attr(data, "groups") %>% names() %>% .[.!=".rows"]
    # Now group the dataset by all grouping variables present
    output <- output %>%
      group_by(across(mygroups))
  }
  
  output %>%
    # Now consolidate intervals so that each row represents one part of the band, so no bands overlap.
    summarize(
      n = c(lower, unique(estimate), upper) %>% length(),
      ymin = c(lower, unique(estimate), upper) %>% sort() %>% .[c(1:(n-1)) ],
      ymax = c(lower, unique(estimate), upper) %>% sort() %>% .[-1],
      ci = c(sort(unique(ci), decreasing = TRUE), sort(unique(ci), decreasing = FALSE))) %>%
    # Add a unique ID for each band segement
    mutate(group = 1:n()) %>%
    # Turn ci into a factor() for easy visualization
    mutate(ci = ci %>% factor(
      # Where the levels are the ordered values of ci
      levels = unique(ci) %>% sort(), 
      # and the labels are the confidence intervals as percentages 
      labels = unique(ci*100) %>% sort() %>% paste(., "%", sep = ""))) %>%
    # consolidate variables
    select(group, ci, lower = ymin, upper = ymax) %>%
    return()
}


