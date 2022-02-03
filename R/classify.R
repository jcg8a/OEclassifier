#' Classify open ended responses from surveys using Jaro-Winkler metric
#'
#' @param raw_data Filepath to a csv or xlsx file
#' @param dictionary Filepath to a csv or xlsx file
#' @param destiny Destiny filepath string. if NULL setwd path will be used
#' @param raw_header TRUE if the raw_data has header
#' @param dic_header TRUE if the dictionary has header
#'
#' @return A tibble
#' @export
#'
#' @examples
classify <- function(raw_data, dictionary, destiny = NULL, raw_header = FALSE, dic_header = FALSE){

  # dictionary loading
  if(stringr::str_detect(dictionary, pattern = ".csv") | stringr::str_detect(dictionary, pattern = ".xlsx")){
    dic <- reader(dictionary, head = dic_header)
    # dic <- dic %>%
      # dplyr::distinct(.keep_all = TRUE) %>%
      # dplyr::mutate(standard = drop_multi(dic[,1])) %>%
      # dplyr::mutate(standard = adjust_str(standard), class = standard)
    dic <- dic %>%
      dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), .fns = drop_multi)) %>%
      dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), .fns = adjust_str)) %>%
      dplyr::distinct(.keep_all = TRUE)

    dic[,"match"] <- dic[,1]
    }
  else{
    print("Aca hay que cargar el diccionario dentro del paquete")
  }

  #raw data loading
  rd <- reader(raw_data, head = raw_header)


  # Quitar duplicados del raw data
  # Quitar los valores a la derecha de la coma, asumiendo que las personas pusieron opciones diferentes luego de cada coma. Este script solo ajusta la primera opcion
  # rd <- rd %>%
    # dplyr::distinct(.keep_all = TRUE) %>%
    # dplyr::mutate(standard = drop_multi(rd[,1])) %>%
    # dplyr::mutate(standard = adjust_str(standard))

  # Creo este id para conservar el orden de los datos originales
  rd.usable <- rd %>%
    dplyr::mutate(id = seq(1,nrow(rd))) %>%
    dplyr::mutate(dplyr::across(.cols = c(1), .fns = drop_multi)) %>%
    dplyr::mutate(dplyr::across(.cols = c(1), .fns = adjust_str))

  # rd.usable <- dplyr::left_join(rd.usable, dic, by= "standard")

  # https://stackoverflow.com/questions/54952338/r-how-to-use-dplyr-left-join-by-column-index
  # rd.usable <- dplyr::left_join(rd.usable, dic, by = c(names(rd.usable)[1] = names(dic)[1])) NO encontre una manera de en vez de usar nombre de columna, usar indice
  rd.usable <- merge(rd.usable, dic, by.x = 1, by.y = 1, all.x = TRUE, all.y = FALSE)

  # Guardo en una tabla auxiliar los match exactos
  rd.exact <- rd.usable %>%
    dplyr::filter(!is.na(match)) %>%
    dplyr::mutate(distance = 0)

  # De la tabal usable me quedo solo con los que no tengan exact match
  rd.usable <- rd.usable %>%
    dplyr::filter(is.na(match)) %>%
    dplyr::select(-match)


  ptm <- proc.time()
  rd.usable <- match_str(rd.usable, dic)
  rd.usable <- dplyr::bind_rows(rd.usable, rd.exact)
  rd.usable <- rd.usable %>% dplyr::arrange(id)
  rd.usable[,1] <- NULL
  rd.usable <- dplyr::bind_cols(rd, rd.usable)
  ptm <- proc.time() - ptm

  # Hasta aca todo esta validado que funciona

  if(is.null(destiny)){
    utils::write.csv(rd.usable)
  }
  else{
    utils::write.csv(rd.usable, file = paste0(destiny,"/OEclasiffier_output.csv"))
  }
  return(rd.usable)
}

utils::globalVariables(c("%>%", "id", "distance"))
# id <- distance <- %>%  <- NULL eso tambien serviria pero prefiero usar la otra version es mas limpia
