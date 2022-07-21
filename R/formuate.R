#' formulate() Function
#'
#' This function quickly updates your model object by adding or subtracting predictor terms, outputting a data.frame ready to be passed to your modeling function of choice.
#' @param model (Required) Model object.
#' @param new (Optional) Character value containing any terms to be added/subtracted from model equation. Defaults to "", meaning no changes.
#' @param data (Optional) If specified, source the data for forming the new model matrix from this data.frame, not the model object. Useful if you want to add a term not currently in your model.
#' @param outcome (Optional) Name of outcome variable. If not written, gathered from model formula
#' @keywords formula
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
#' # Update regression model using formulate
#' 
#' # Add an interaction between two variables already in the model
#' mymodel %>%
#'  formulate(" + disp*factor(cyl)")
#' 
#' # Now run the model by piping in the model function afterwards
#' mymodel %>%
#'  formulate(" + disp*factor(cyl)") %>%
#'  lm()
#'
#' # Or, add in a variable that wasn't in the original model, like wt
#' # by adding the raw dataframe in under the 'data' parameter 
#' mymodel %>%
#'   formulate(" + disp*wt", mtcars, outcome = "drat")
#'
#' # Or, change the outcome variable TOO, adding a variable that wasn't in the original model, like drat
#' mymodel %>%
#'   formulate(" + disp*wt", mtcars, outcome = "drat")
#' 
#' # Happy modeling!
#'

formulate = function(model, new = "", data = NULL, outcome = NULL){
  
  # If there's no outcome term specified,
  if(is.null(outcome)){
    # Extract term name of model outcome
    outcome <- model$terms %>%  
      attr("dataClasses") %>%
      attr("names") %>%
      .[1]
    
  }
  # Extract vector of all terms
  terms <- model$terms %>% attr("term.labels") %>%
    paste(collapse = " + ")
  
  # Take the outcome
  newformula <- outcome %>%
    # And paste it together with the terms and update
    paste(., " ~ ", terms, new, sep = " ") %>%
    # Convert character to formula
    as.formula()
  
  # If a dataset IS specified,
  if(!is.null(data)){
  # Return model matrix, formatted to contain the variables from the new formula only
  mymatrix <- modelr::model_matrix(data, formula = newformula) %>%
    # Drop intercept column; lm will add it in automatically
    select(-1) %>%
    # Add in outcome
    tibble(!!sym(outcome) := data[,outcome], 
           ., rownames = rownames(data) )
  }else{
    
  # If a dataset is NOT specified, just source everything from the model object
  mymatrix <- modelr::model_matrix(model$model, formula = newformula) %>%
    # Drop intercept column; lm will add it in automatically
    select(-1) %>%
    # Add in outcome
    tibble(!!sym(outcome) := model$model[,outcome], 
           ., rownames = rownames(model$model) )
  }
  
  # Export  
  mymatrix %>%
    # Convert rowname column to an actual rowname
    column_to_rownames(var = "rownames") %>%
    # Keep rownames
    return()
}
