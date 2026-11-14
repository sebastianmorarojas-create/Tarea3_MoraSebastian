#' Cuenta alertas diarias comparando cada día con los días previos.
#'
#' @param df data.frame con las columnas fecha e id_tx.
#' @param ventana número entero de días previos usados en el cálculo.
#' @param k.alerta multiplicador no negativo de la desviación estándar.
#' @return data.frame diario con fecha, n.tx, media, desv y alerta.
alerta.movil <- function(df, ventana, k.alerta) {
  if (!is.data.frame(df)) {
    stop("df debe ser un data.frame.")
  }
  if (!all(c("fecha", "id_tx") %in% names(df))) {
    stop("df debe contener las columnas fecha e id_tx.")
  }
  if (length(ventana) != 1L || !is.numeric(ventana) || is.na(ventana) ||
      ventana < 2 || ventana != as.integer(ventana)) {
    stop("ventana debe ser un entero de al menos 2.")
  }
  if (length(k.alerta) != 1L || !is.numeric(k.alerta) ||
      is.na(k.alerta) || !is.finite(k.alerta) || k.alerta < 0) {
    stop("k.alerta debe ser un número finito no negativo.")
  }
  if (nrow(df) == 0L) {
    stop("df no contiene transacciones.")
  }
  
  fecha <- as.Date(df$fecha)
  if (anyNA(fecha)) {
    stop("fecha contiene valores que no se pueden convertir a Date.")
  }
  
  conteos <- aggregate(df$id_tx, by = list(fecha = fecha), FUN = length)
  names(conteos)[2L] <- "n.tx"
  dias <- seq.Date(min(fecha), max(fecha), by = "day")
  n.tx <- conteos$n.tx[match(dias, conteos$fecha)]
  n.tx[is.na(n.tx)] <- 0L
  
  n.dias <- length(dias)
  media <- rep(NA_real_, n.dias)
  desv <- rep(NA_real_, n.dias)
  alerta <- rep(NA, n.dias)
  
  if (n.dias > ventana) {
    for (i in seq.int(ventana + 1L, n.dias)) {
      previos <- n.tx[seq.int(i - ventana, i - 1L)]
      media[i] <- mean(previos)
      desv[i] <- stats::sd(previos)
      alerta[i] <- n.tx[i] > media[i] + k.alerta * desv[i]
    }
  }
  
  data.frame(
    fecha = dias,
    n.tx = n.tx,
    media = media,
    desv = desv,
    alerta = alerta
  )
}

#' Calcula la desviación absoluta media de los primeros dígitos.
#'
#' @param x vector numérico de montos.
#' @return valor numérico MAD según la ley de Benford.
benford.mad <- function(x) {
  if (!is.numeric(x)) {
    stop("x debe ser un vector numérico.")
  }
  montos <- x[is.finite(x) & x > 0]
  if (length(montos) == 0L) {
    stop("x debe contener al menos un monto positivo y finito.")
  }

  digitos <- floor(montos / 10^floor(log10(montos)))
  observadas <- tabulate(as.integer(digitos), nbins = 9L) / length(digitos)
  esperadas <- log10(1 + 1 / seq_len(9L))

  mean(abs(observadas - esperadas))
}

#' Resume la desviación de Benford para cada cantón.
#'
#' @param df data.frame con cod_canton, canal y monto_crc.
#' @param canal nombre del canal que se desea analizar.
#' @param cantones data.frame con cod_canton y canton.
#' @return data.frame ordenado de mayor a menor MAD.
benford.canton <- function(df, canal, cantones) {
  requeridas <- c("cod_canton", "canal", "monto_crc")
  if (!is.data.frame(df) || !all(requeridas %in% names(df))) {
    stop("df debe contener cod_canton, canal y monto_crc.")
  }
  if (length(canal) != 1L || is.na(canal) || !nzchar(canal)) {
    stop("canal debe indicar un único canal.")
  }
  if (!is.data.frame(cantones) ||
      !all(c("cod_canton", "canton") %in% names(cantones))) {
    stop("cantones debe contener cod_canton y canton.")
  }

  datos <- df[df$canal == canal & is.finite(df$monto_crc) & df$monto_crc > 0, ]
  if (nrow(datos) == 0L) stop("No hay montos válidos para el canal indicado.")
  codigos <- unique(cantones$cod_canton)
  salida <- lapply(codigos, function(codigo) {
    montos <- datos$monto_crc[datos$cod_canton == codigo]
    nombre <- cantones$canton[match(codigo, cantones$cod_canton)]
    if (length(montos)) {
      mad <- benford.mad(montos)
      clase <- if (mad <= 0.006) "Conformidad cercana" else if (mad <= 0.012) {
        "Conformidad aceptable"
      } else if (mad <= 0.015) "Conformidad marginal" else "No conformidad"
    } else {
      mad <- NA_real_
      clase <- "Sin datos"
    }
    data.frame(cod_canton = codigo, canton = nombre, n = length(montos),
               mad = mad, clase = clase, stringsAsFactors = FALSE)
  })
  resultado <- do.call(rbind, salida)
  resultado[order(resultado$mad, decreasing = TRUE, na.last = TRUE), ]
}
