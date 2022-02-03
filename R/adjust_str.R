#' Drop leading and trailing whitespaces, punctuation signs and capitalize strings.
#'
#' @param x A string
#'
#' @return A string
#'
#' @examples
#' x <- "cat#!"
adjust_str <- function(x){

  x <- x %>% stringr::str_trim("both") %>%
    stringr::str_replace_all("[:punct:]","") %>%
    stringr::str_to_upper(locale = "en")

  return(x)
}

# adjust_str(x)
