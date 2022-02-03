#' Import csv or xlsx files
#'
#' @param path filepath
#' @param head TRUE if the file to import has a first row of headers
#'
#' @return A data frame
#'
#' @examples
reader <- function(path, head){

  # path <- stringr::str_replace(string = path, pattern = "\\", replacement = "/")

  if(stringr::str_sub(path, start = -3, end = -1) == "csv"){
    x <- utils::read.csv(path, header = head, encoding = "UTF-8", stringsAsFactors = FALSE)
    return(x)
  }
  if(stringr::str_sub(path, start = -4, end = -1) == "xlsx"){

  }
  else{
    print("Este formato no esta soportado")
  }
}
