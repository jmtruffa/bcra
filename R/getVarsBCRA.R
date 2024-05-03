#' getPrincipalesVariables
#'
#' @description
#'  Obtiene las principales variables del BCRA
#'
#'  @return un tibble con las principales variables del BCRA
getPrincipalesVariables <- function() {
  require(httr2)
  require(tidyverse)
  require(jsonlite)
  url = "https://api.bcra.gob.ar/estadisticas/v1/PrincipalesVariables"


  response = request(url) %>%
    req_method("GET") %>%
    req_perform()
  data = fromJSON(rawToChar(response$body))
  result = as_tibble(data$results)
  return(result)
}

#' getDatosVariable
#'
#' @description Obtiene los datos de una variable del BCRA
#'
#' @param idVariable id de la variable a obtener
#' @param desde fecha desde la cual se quiere obtener los datos
#' @param hasta fecha hasta la cual se quiere obtener los datos
#'
#' @return un tibble con los datos de la variable
getDatosVariable <- function(idVariable, desde, hasta) {
  require(httr2)
  require(tidyverse)
  require(jsonlite)
  url <- paste0("https://api.bcra.gob.ar/estadisticas/v1/datosvariable/", idVariable, "/", desde, "/", hasta)

  response = request(url) %>%
    req_headers(`Accept_Language` = "en-US") %>%
    req_method("GET") %>%
    req_perform()


  body = fromJSON(rawToChar(response$body))$results
  body$valor = as.double(gsub(",", ".", body$valor))
  body$date = as.Date(body$fecha, format = "%d/%m/%Y")
  body = body %>% select(date, valor)
  return(tibble(body))
}

