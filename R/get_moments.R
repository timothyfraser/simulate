#' get_moments() Function
#'
#' This function uses the method of moments to approximate alpha and beta parameters in a beta-distributed simulation.
#' @param d (Optional) A numeric vector. If supplied, used to calculate `mu` and `var`.
#' @param mu (Optional) A single numeric value, representing the mean of beta distribution. Requires `var` simultaneously.
#' @param var (Optional) A single numeric value, representing the variance of beta distribution. Requires `mu` simultaneously.
#' @keywords moments
#' @export

get_moments = function(d = NULL, mu = NULL, var = NULL){
  
  # If d is specified, calculate ex and vx from d
  if(!is.null(d)){
    # Get mean of d
    mu <- d %>% mean()
    # Get variance of d
    var <- d %>% var()
  }else{
    # If d is NOT specified, use the supplied ex and vx
  }
  
  
  a = (((mu * (1 - mu)) / var) - 1) * mu
  b = (mu*(1 - mu)/var - 1)*(1 - mu)
  # Return!
  list(a, b) %>%
    `names<-`(c("shape1", "shape2")) %>%
    return()
}