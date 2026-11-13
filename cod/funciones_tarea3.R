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
