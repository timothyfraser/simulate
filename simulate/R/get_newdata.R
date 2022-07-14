#' get_newdata() Function
#'
#' This wrapper function extracts the median/modal observations for a set of data,
#' @param m Model object. Required.
#' @param setx named list of hypothetical predictor values, for any variables of interest. All others set to median (for numeric variables) or mode (for categorical variables) by default. Note: This package currently cannot handle factor() language in model formulas; please transform any relevant variables to factors before modeling. 
#' @keywords average mode
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
#' # Get a data.frame of the average respondent, if just the following variable values were changed
#' newdata <- get_newdata(m = mymodel, setx = list(disp = c(1,2,3)))
#' 

get_newdata = function(m, setx = NULL){
  # Let's write a quick function called get_newdata
  
  # Require a model input
  if(is.null(m)){stop()}
  
  # Let's get the median observation by default
  myaverages <- m %>% 
    # Extract data
    model.frame() %>%
    # Cut the outcome variable
    select(-1) %>%
    summarize(across(where(is.factor) | where(is.character), ~get_mode(.x)),
              across(where(is.numeric) | where(is.double), ~median(.x, na.rm = TRUE)))
  # After setting these to the modal / median categories,
  if(is.null(setx)){ 
    # Just return as is if no specific constraints
    myaverages %>% return()  
  }else{
    # If user supplied any specific constraints, override the variables with this list of values
    # Take myaverages
    myaverages %>%
      # and put it into a new data.frame with the contents of setx,
      # overwriting any overlapping variables from myavverages
      # and stretching it to the length of setx.
      summarize(myaverages, setx %>% as_tibble()) %>%
      return()
  }
}





  