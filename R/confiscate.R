#' confiscate() Function
#'
#' This function identifies and cuts `n` outliers from your model, then uses [formulate()] to return a data.frame that can be remodeled.
#' @param model (Required) Model object.
#' @param n (Optional) Number of highest outliers to cut, as measured by the highest Cook's distance scores. 
#' @param cutoff (Optional) Numeric value stating score of Cook's distance *after* which outliers will be cut.
#' @keywords outliers
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
#' # Cut five highest outliers from the dataset
#' mymodel %>% 
#'  confiscate(n = 5) %>%
#'  lm()
#' 
#' # Alternatively, we could cut off any cases with a Cook's D higher than `cutoff`
#' # In our case, the highest cooks distance is about 0.15, 
#' # so we'll arbitrarily set the `cutoff` to 0.1 to demonstrate.
#' mymodel %>% 
#'  confiscate(cutoff = 0.1) %>%
#'  lm()


confiscate = function(model, n = 5, cutoff = NULL){
  # Calculate cook's D  
  cooks_d <- cooks.distance(model)
  
  if(is.null(cutoff)){
    subset <- cooks_d %>% 
      # Sort them from highest to lowest
      sort(TRUE) %>% 
      # Grab the 1 to nth highest 
      .[1:n] %>% 
      # Get the rownames
      names()
  }else{
    # If the cutoff IS specified,
    # then filter cooks_d to those cases with cooks d higher than cutoff
    subset <- cooks_d[cooks_d > cutoff] %>%
      # get the rownmaes
      names()
  }
  
  all <- rownames(model$model)
  
  remainder <- all[!all %in% subset]
  
  model %>%
    # Reformulate the model (as is)
    formulate() %>%
    .[remainder, ] %>%
    return()
}

