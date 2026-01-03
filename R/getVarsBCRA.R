#' getPrincipalesVariables
#'
#' @description Obtiene el catlogo de variables del BCRA (v4.0; pagina si hace falta)
#' @return un tibble con el catlogo de variables
getPrincipalesVariables <- function() {
  require(httr2)
  require(tidyverse)
  require(jsonlite)
  url = "https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias"


  response = request(url) %>%
    req_method("GET") %>%
    req_perform()
  data = fromJSON(rawToChar(response$body))
  result = as_tibble(data$results)
  return(result)
}

#' int_getDatosVariable
#'
#' @description Obtiene la serie de una variable del BCRA entre fechas (v4.0)
#' @param idVariable id de la variable a obtener
#' @param desde fecha desde la cual se quiere obtener los datos (YYYY-MM-DD)
#' @param hasta fecha hasta la cual se quiere obtener los datos (YYYY-MM-DD)
#' @return un tibble con columnas date y valor
int_getDatosVariable <- function(idVariable, desde, hasta) {
  require(httr2)
  require(tidyverse)
  require(jsonlite)

  url = paste0("https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias/",
               idVariable, "?desde=", desde, "&hasta=", hasta)

  response = request(url) %>%
    req_headers(Accept_Language = "en-US") %>%
    req_method("GET") %>%
    req_perform()

  parsed = fromJSON(rawToChar(response$body))

  # Verificar si hay resultados
  if (is.null(parsed$results) ||
      is.null(parsed$results$detalle) ||
      length(parsed$results$detalle) == 0) {
    # Devolver tibble vacío con la estructura esperada
    return(tibble(date = as.Date(character()), valor = numeric()))
  }

  detalle = parsed$results$detalle

  # Si detalle es una lista (1 observación), convertir a data.frame
  if (is.list(detalle) && !is.data.frame(detalle)) {
    detalle = as.data.frame(detalle)
  }

  # Convertir a tibble y verificar columnas
  body = as_tibble(detalle)

  # Verificar que existan las columnas necesarias
  if (!"valor" %in% names(body) || !"fecha" %in% names(body)) {
    return(tibble(date = as.Date(character()), valor = numeric()))
  }

  # Procesar y seleccionar solo fecha y valor
  body = body %>%
    mutate(
      valor = as.double(gsub(",", ".", valor)),
      fecha = as.Date(fecha, format = "%Y-%m-%d")
    ) %>%
    select(date = fecha, valor)

  return(body)
}

#' getDatosVariable
#'
#' @description Wrapper que respeta la restriccin de periodo (mx. ~1 ao por hit).
#' Rompe en tramos anuales y concatena.
#' @param idVariable id de la variable
#' @param desde fecha (YYYY-MM-DD)
#' @param hasta fecha (YYYY-MM-DD)
#' @return tibble con (date, valor)
getDatosVariable <- function(idVariable, desde, hasta) {
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

