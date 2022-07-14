#' get_mode() Function
#'
#' This function grabs the modal response from a vector.
#' @param x A numeric, categorical, or factor vector.
#' @keywords mode
#' @export
#' @examples
#' 
#' c("cat", "dog", "dog", "mouse", NA_character_) %>%
#'    get_mode()
#'

get_mode = function(x){
  x %>%
    # Drop NAs
    .[!is.na(.)] %>%
    # count up
    table() %>%
    # order from highest to lowest
    sort(decreasing = TRUE) %>%
    # get item name
    names() %>%
    # Return first
    .[1] %>%
    return()
}
