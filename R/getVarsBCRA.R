#' getPrincipalesVariables
#'
#' @description Obtiene el catlogo de variables del BCRA (v4.0; pagina si hace falta)
#' @return un tibble con el catlogo de variables
getPrincipalesVariables <- function() {
  require(httr2)
  require(jsonlite)
  require(dplyr)
  require(tibble)

  base_url <- "https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias"

  fetch_page <- function(offset = 0, limit = 1000) {
    req <- request(base_url) |>
      req_url_query(offset = offset, limit = limit) |>
      req_headers(`Accept-Language` = "en-US") |>
      req_method("GET")

    resp <- req_perform(req)
    out  <- jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = TRUE)
    out
  }

  pg1 <- fetch_page(0, 1000)
  res <- as_tibble(pg1$results)

  total <- tryCatch(pg1$metadata$resultset$count, error = function(e) nrow(res))
  limit <- tryCatch(pg1$metadata$resultset$limit, error = function(e) 1000L)

  offset <- limit
  while (!is.na(total) && offset < total) {
    pg <- fetch_page(offset, limit)
    res <- bind_rows(res, as_tibble(pg$results))
    offset <- offset + limit
  }

  res |>
    select(
      idVariable, descripcion, categoria, tipoSerie, periodicidad,
      unidadExpresion, moneda,
      primerFechaInformada, ultFechaInformada, ultValorInformado
    ) |>
    arrange(idVariable)
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
  require(jsonlite)
  require(dplyr)
  require(tibble)

  stopifnot(!missing(idVariable), !missing(desde), !missing(hasta))

  base_url <- paste0("https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias/", idVariable)

  req <- request(base_url) |>
    req_url_query(desde = desde, hasta = hasta) |>
    req_headers(`Accept-Language` = "en-US") |>
    req_method("GET")

  resp <- req_perform(req)
  body <- jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = TRUE)

  # Estructura v4 esperada: results$detalle = lista/df con campos fecha, valor
  det <- NULL
  if (!is.null(body$results$detalle)) {
    det <- body$results$detalle
    # si viene anidado como lista de una sola entrada
    if (is.list(det) && length(det) == 1 && is.data.frame(det[[1]])) det <- det[[1]]
  } else if (!is.null(body$results)) {
    # fallback: algunas respuestas viejas devolvan results$fecha/valor directo
    det <- body$results
  } else {
    stop("Respuesta de API sin campo 'results'.")
  }

  det <- as_tibble(det) |>
    rename(date = any_of(c("fecha", "Date", "date")),
           valor = any_of(c("valor", "Value", "value"))) |>
    select(date, valor)

  # Normalizacin robusta de tipos
  det <- det |>
    mutate(
      date  = as.Date(as.character(date)),
      valor = suppressWarnings(as.numeric(gsub(",", ".", as.character(valor))))
    ) |>
    arrange(date)

  det
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
  require(dplyr)
  require(tibble)

  desde <- as.Date(desde)
  hasta <- as.Date(hasta)
  stopifnot(!is.na(desde), !is.na(hasta), desde <= hasta)

  out <- list()
  current_start <- desde

  while (current_start <= hasta) {
    # fin del tramo: ltimo da del ao de current_start o 'hasta', lo que ocurra primero
    tramo_end <- as.Date(paste0(format(current_start, "%Y"), "-12-31"))
    current_end <- min(tramo_end, hasta)

    chunk <- int_getDatosVariable(
      idVariable = idVariable,
      desde = format(current_start, "%Y-%m-%d"),
      hasta = format(current_end, "%Y-%m-%d")
    )

    out <- append(out, list(chunk))
    current_start <- current_end + 1
  }

  bind_rows(out) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)
}

