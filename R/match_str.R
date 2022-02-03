#' Match the closest string using the Jaro-Winkler metric from stringdist package
#'
#' @param x A data frame or tibble of strings to match
#' @param y A data frame or tibble of strings to use as references
#'
#' @return A tibble!!!!!!!!
#'
#' @examples
match_str <- function(x, y){
  for(i in 1:nrow(x)){
    aux_y <- y
    for(j in 1:nrow(y)){
      # aux_y[j, "distance"] <- stringdist::stringdist(as.character(x[i, "standard"]), as.character(y[j, "standard"]), method = "jw")
      aux_y[j, "distance"] <- stringdist::stringdist(as.character(x[i, 1]), as.character(y[j, 1]), method = "jw")
      aux_y <- aux_y %>%
        dplyr::arrange(distance)
    }
    x[i, 3:4] <- aux_y[1, 2:3]
  }
  return(x)
}

