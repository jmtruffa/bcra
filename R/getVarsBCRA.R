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
  url = "https://api.bcra.gob.ar/estadisticas/v2.0/PrincipalesVariables"


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
  url <- paste0("https://api.bcra.gob.ar/estadisticas/v2.0/datosvariable/", idVariable, "/", desde, "/", hasta)

  response = request(url) %>%
    req_headers(`Accept_Language` = "en-US") %>%
    req_method("GET") %>%
    req_perform()


  body = fromJSON(rawToChar(response$body))$results[2:3]
  body$valor = as.double(gsub(",", ".", body$valor))
  body$fecha = as.Date(body$fecha, format = "%Y-%m-%d")
  body = body %>% select(date = fecha, valor)
  return(tibble(body))
}

