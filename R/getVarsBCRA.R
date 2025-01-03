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
  url = "https://api.bcra.gob.ar/estadisticas/v3.0/Monetarias"


  response = request(url) %>%
    req_method("GET") %>%
    req_perform()
  data = fromJSON(rawToChar(response$body))
  result = as_tibble(data$results)
  return(result)
}

#' _getDatosVariable
#'
#' @description Obtiene los datos de una variable del BCRA
#'
#' @param idVariable id de la variable a obtener
#' @param desde fecha desde la cual se quiere obtener los datos
#' @param hasta fecha hasta la cual se quiere obtener los datos
#'
#' @return un tibble con los datos de la variable
int_getDatosVariable = function(idVariable, desde, hasta) {
  require(httr2)
  require(tidyverse)
  require(jsonlite)
  url = paste0("https://api.bcra.gob.ar/estadisticas/v3.0/Monetarias/", idVariable, "?desde=", desde, "&hasta=", hasta)

  response = request(url) %>%
    req_headers(`Accept_Language` = "en-US") %>%
    req_method("GET") %>%
    #req_dry_run()
    req_perform()


  body = fromJSON(rawToChar(response$body))$results[2:3]
  body$valor = as.double(gsub(",", ".", body$valor))
  body$fecha = as.Date(body$fecha, format = "%Y-%m-%d")
  body = body %>% select(date = fecha, valor)
  return(tibble(body))
}

#' getDatosVariable
#'
#' @description Wrapper que simplifica la periodicidad máxima que
#' impuso el BCRA en los hits a la API (1 año). Llama a la función
#' interna ("_") con su mismo nombre que es la que pega a la API.
#'
#' @param idVariable id de la variable a obtener
#' @param desde fecha desde la cual se quiere obtener los datos
#' @param hasta fecha hasta la cual se quiere obtener los datos
#'
#' @return un tibble con los datos de la variable
getDatosVariable = function(idVariable, desde, hasta) {
  # Convert the dates to Date class
  desde <- as.Date(desde)
  hasta <- as.Date(hasta)

  # Initialize an empty list to store the results
  all_data <- list()

  # Loop through the date range and make API calls in 1-year intervals
  current_start <- desde
  while (current_start <= hasta) {
    # Define the end date for the current interval
    #current_end = min(current_start + 364, hasta)  # 364 to ensure it doesn't exceed 1 year
    next_year_end <- as.Date(paste0(as.numeric(format(current_start,"%Y")) + 1, "-12-31"))
    current_end <- min(next_year_end, hasta)

    # Make the API call
    data <- bcra::int_getDatosVariable(idVariable = idVariable,
                                   desde = as.character(current_start),
                                   hasta = as.character(current_end))

    # Store the result
    all_data = append(all_data, list(data))

    # Update the start date for the next interval
    current_start = current_end + 1
  }

  # Combine all the results into a single data frame
  combined_data = do.call(rbind, all_data) %>% arrange(date)

  return(combined_data)
}
