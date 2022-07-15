#' equate() Function
#'
#' This function allows you to generate a multivariate normal distribution
#' @param m Model object. Required.
#' @param reps Number of simulations to generate of coefficients. Defaults to 1000.
#' @param seed Seed for perfect replication of simulations. Defaults to 12345 unless otherwise specified.
#' @keywords simulate
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
#' d <- equate(m = mymodel)
#' 
#' # Check out the first 5 lines below. Notice the unique ID for each row, listed under replicate.
#' head(d)
#' 

equate = function(m = NULL, 
                     reps = 1000, seed = 12345){
  require(stats)
  require(broom)
  
  # If model is not supplied, then...
  if(is.null(m)){
    # Add message
    print("No model supplied.")
    stop()
  }else{
    # Get name of model type
    if(class(m) %in% c("lm")){ type <- "lm" }
    if(class(m) == "glm"){ type <- "glm"}
    if(family(m)$family %in% c("logit")){ type <- "logit" }
    if(family(m)$family %in% c("poisson")){ type <- "poisson"}  
    if(family(m)$family == "Gamma"){ type <- "gamma" }
    if(family(m)$family %>% str_detect(pattern = "Negative Binomial")){ type <- "nb"}
    if(class(m) %in% c("betareg")){ type <- "betareg"}
    
    # Get variance covariance matrix
    myvcov <- vcov(m)
    
    # Get model coefficients in manner appropriate for each model    
    mycoef <- c(NA_real_) # by default
    
    # If most models, extract like so
    if(type %in% c("lm", "glm", "logit", "poisson", "negbin", "gamma")){
      mycoef <- m$coefficients
    }
    # If beta regression, extract this way
    if(type == "betareg"){
      mycoef <- c(m$coefficients$mean, m$coefficients$precision)
    }
    
  }
  
  # Get degrees of freedom and sample size too
  nrows <- nrow(m$model)
  nvars <- length(m$model)
  
  
  # If user supplies a specific seed, use it!
  # Otherwise, the seed for replication is always 12345.
  if(is.null(seed)){seed = 12345}
  set.seed(seed)
  
  # Construct multivariate normal distribution of coefficients,
  # representing estimation uncertainty
  MASS::mvrnorm(
    n = reps, # get 1000 simulations per imputation
    # get vector of our coefficients
    mu = mycoef,
    # get variance-covariance matrix
    Sigma = myvcov) %>%
    as_tibble() %>%
    # Add an ID row
    bind_cols(tibble(replicate = 1:reps), .) %>%
    # Finally, add some metadata as an attribute    
    structure(.Data = ., 
              # that metadata will be...
              # the model coefficients themselves
              coefficients = mycoef,
              nrows = nrows,
              nvars = nvars,
              # And encode the literal model too
              model = m,
              # Encode model type
              type = type,
              # Encode seed
              seed = seed,
              step = "equations") %>%
    return()
  
}



