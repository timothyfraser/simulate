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
  
  # Add some helpfer functions
  
  # Polynomials
  # Is it a polynomial variable? Check!
  is.poly = function(x){"poly" %in% class(x)}
  
  # Let's write a quick function to
  # return the original vector from a polynomial transformation
  unpoly = function(p){
    
    # Get coefficients of polynomial
    c <- p %>% attr("coefs")
    
    # If it's a raw polynomial 'p', that's easy!
    # It will have a null value for c, aka attr(p, "coefs")
    if(is.null(c)){
      # Just grab the first column and return it
      x <- p[,1]
      return(x)
    }else{
      # For an orthagonal polynomial 'p'
      # It will have coefficients and will NOT be null
      
      # Get the normalizing constnats
      norm2 <- c$norm2
      # Get the means
      alpha <-c$alpha
      # Get scaling factors
      scale <- norm2[-c(1:2)] %>% sqrt()
      
      # Now, multiply by the normalizing constants
      # to return to original scale,
      # and then add alpha (the mean),
      # to fix the fact that it was de-meaned
      x <- p * rep(scale, each = nrow(p)) + alpha
      # Return the original vector
      return(x[,1])
    }
  }
  
  name.poly = function(x){
    xname <- x %>%
      # Remove any of these terms, if used
      # Did they specify polynomial?
      str_remove("poly[(]") %>%
      # Did they specify x explicitly?
      str_remove("[x][ ]?+[=][ ]?+") %>%
      # Did they specify raw/orthogonal polynomial?
      str_remove("raw[ ]?+[=][ ]?+(TRUE|FALSE|T|F|1|0)?[,]?[ ]?+") %>%
      # Did they specify degree (usually they had to)
      str_remove("degree[ ]?+[=][ ]?+[0-9]+[,]?[ ]?+") %>%
      # If there are coefs, keep removing things 
      # until you hit (1) an end parenthesis, comma, and optional space
      str_remove("coefs[ ]?+[=][ ]?+.*.[)][,][ ]?+") %>%
      # Did they specify simple?
      str_remove("simple[ ]?+[=][ ]?+(TRUE|FALSE|T|F|1|0)?[,]?[ ]?+") %>%
      # If they didn't specify the argument, eg. degree, coefs, etc.
      # Then remove any remaining that follow this pattern,
      # and replace with just a comma
      str_remove("[,][ ]?+([0-9]+|TRUE|FALSE|T|F)?[ ]?+") %>%
      # Having removed the other inputs to poly,
      # there should only be a comma, optional space, and parentheses left
      # remove these.
      str_remove("[,]?[ ]?[)]")
    
    # Print a warning
    paste(
      "Polynomial Used. Approximating Name of Variable as: ", xname, 
      ". Please fix if incorrect.", sep = "") %>%
      print()
    
    # Return the most likely name of the vector
    return(xname)
    
  }
  
  # Need to remove two things on either side of something?
  # Specify the two things and remove them with this function.
  str_remove_between = function(x, a, z){
    # Get everything between a and z in vector x
    all <- str_extract(x, paste(a, ".+", z, sep = ""))
    # Remove a then z, leaving what's between
    out <- all %>% str_remove(a) %>% str_remove(z)
    
    x %>% 
      str_replace(
        # Find every case of a-out-z,
        pattern = paste(a, out, z, sep = ""),
        # and replace it with just 'out'
        replacement = out) %>%
      return()
  }
  
  # Get the original name of a variable that was within log() or sqrt() 
  # (and sometimes works for I())
  name.i = function(x){
    # Take the name of the variable beginning with i
    xname <- x %>%
      str_remove("I[(]") %>%
      # Did they specify x explicitly?
      str_remove("[x][ ]?+[=][ ]?+") %>%
      # Remove any exponents,
      # including ^.123423, ^2, or ^(1/3)
      str_remove("[\\^][ ]?+([0-9]+|[(][0-9]+[/][0-9][)]|[0-9]+\\.[0-9]+|\\.[0-9]+)") %>%
      # Remove special terms with parentheses, including
      # Square Root
      str_remove_between(a = "sqrt[(]", z = "[)]") %>%
      # Log transformation
      str_remove_between(a = "log[(]", z = "[)]") %>%
      # Common Operations
      str_remove_between(a = "mean[(]", z = "[)]") %>%
      str_remove_between(a = "sum[(]", z = "[)]") %>%
      str_remove_between(a = "sd[(]", z = "[)]") %>%
      # And if you've used anything else....
      # Sad. Please try to do these edits directly 
      # to your data frame before running your model.  
      # Having removed the other inputs to poly,
      # there should only be a comma, optional space, and parentheses left
      # remove these.
      str_remove("[,]?[ ]?[)]")
    
    paste(
      "I(), log(), or sqrt() Used. Approximating Names of Variables", xname, 
      ". Please fix if incorrect.", sep = "") %>%
      print()
    
    # Return the most likely name of the vector
    return(xname)
  }
  
  
  # Require a model input
  if(is.null(m)){stop()}
  
  # Let's get the median observation by default
  myaverages <- m %>% 
    # Extract data
    model.frame() %>%
    # Cut the outcome variable
    select(-1) %>%
    # If any variables are polynomials, fix their column names
    rename_if(.predicate = ~is.poly(.), ~name.poly(.)) %>%
    # If any variables are polynomials, extract the original variable
    mutate(across(where(is.poly), ~unpoly(.x))) %>%
    # If any variables have been logged, sqrted, or I() transformed
    # in the model formula itself, rename them here.
    # For logged variables, exponentiate them
    mutate(across(.cols = contains("log("), -contains("I("), .fns = ~exp(.x))) %>%
    # For sqrted variables, square it
    mutate(across(.cols = contains("sqrt("), -contains("I("), .fns = ~.x^2)) %>%
    # Now rename those variables with our best guess of the variable name,
    # or tell user otherwise using the 'name.i' function.
    rename_with(contains("sqrt("), contains("log(") -contains("I("),
                .fn = ~name.i(.)) %>%
    # Now extract median or mode
    summarize(
      across(where(is.factor) | where(is.character), ~get_mode(.x)),
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





  