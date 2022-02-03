#' Drop strings to the left of the first comma
#'
#' @param x A character.
#'
#' @return A character.
#'
#' @examples
#' x <- "cat, dog"
drop_multi <- function(x){
  x <- gsub(",.*", "", x)
  return(x)
}
