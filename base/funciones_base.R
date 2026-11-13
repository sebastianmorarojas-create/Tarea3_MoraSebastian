# ============================================================
# funciones_base.R  ·  CA0204 · Tarea integradora
# Lo usan: 00_generar_base.R (estudiantes) y calificar.R (docente)
# NO MODIFICAR: cambiar una sola línea cambia la base de cada persona
# y sus respuestas dejarían de coincidir con la clave.
# ============================================================

UMBRAL_CRC <- 2000000   # umbral de la pregunta R02 (monto_crc >= UMBRAL_CRC)

CANALES <- c("App", "SINPE M\u00f3vil", "Web", "Cajero", "Sucursal")

fijar_semilla <- function(s) {
  set.seed(s, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
}

# Convierte un carné (ej. "C12345") en un número entero reproducible
carne_a_semilla <- function(carne) {
  x <- utf8ToInt(toupper(gsub("[[:space:]-]", "", carne)))
  as.integer(sum(x * seq_along(x)^2) %% 2147483629)
}

# Genera la base de transacciones de una persona (misma carné = misma base)
generar_transacciones <- function(carne, cantones) {
  semilla <- carne_a_semilla(carne)
  nc <- nrow(cantones)

  # 1) Peso base de cada cantón: igual para todas las personas
  fijar_semilla(2026)
  w_base <- exp(rnorm(nc, 0, 0.9))
  w_base <- w_base * ifelse(cantones$provincia == "San Jos\u00e9", 2.2,
                     ifelse(cantones$provincia %in% c("Alajuela", "Heredia", "Cartago"), 1.3, 1))

  # 2) Todo lo demás depende del carné
  fijar_semilla(semilla)
  n         <- 1100000L + as.integer(semilla %% 200000L)   # siempre > 1 048 576
  w_canton  <- w_base * exp(rnorm(nc, 0, 0.15))
  mult_cant <- exp(rnorm(nc, 0, 0.20))
  w_mes     <- exp(rnorm(12, 0, 0.12))
  canal_p   <- c(0.34, 0.31, 0.14, 0.11, 0.10)
  canal_med <- c(51000, 20200, 120000, 52800, 166000) * exp(rnorm(5, 0, 0.30))
  sdlog     <- 1.0

  dias  <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  mes_d <- as.integer(format(dias, "%m"))
  fecha <- sort(sample(dias, n, replace = TRUE, prob = w_mes[mes_d]))
  cod   <- sample(cantones$cod_canton, n, replace = TRUE, prob = w_canton)
  ic    <- sample.int(5L, n, replace = TRUE, prob = canal_p)
  z     <- rnorm(n)
  monto <- exp(log(canal_med)[ic] - 0.5 * sdlog^2 + sdlog * z) *
           mult_cant[match(cod, cantones$cod_canton)]
  monto <- pmin(pmax(round(monto, -2), 100), 2e9)

  # id_tx: consecutivo, con algunos repetidos "sembrados" (calidad de datos)
  id_tx <- 1000000L + seq_len(n)
  k   <- 20L + as.integer(semilla %% 41L)
  idx <- sample(2:n, k)
  id_tx[idx] <- id_tx[idx - 1L]

  data.frame(id_tx = id_tx, fecha = fecha, cod_canton = cod,
             canal = CANALES[ic], monto_crc = as.integer(monto),
             stringsAsFactors = FALSE)
}

# ---- Tarea 3: parámetros personales (salen del carné) ----
parametros.tarea3 <- function(carne) {
  s <- carne_a_semilla(carne)
  data.frame(
    parametro = c("carne", "ventana", "k.alerta", "canal.benford",
                  "tasa.anual", "canal.interes"),
    valor = c(toupper(carne),
              c(5, 7, 10, 14)[s %% 4 + 1],
              c(1.5, 1.75, 2, 2.25)[(s %/% 4) %% 4 + 1],
              CANALES[(s %/% 16) %% 5 + 1],
              c(0.04, 0.05, 0.06, 0.07, 0.08, 0.09)[(s %/% 80) %% 6 + 1],
              CANALES[(s %/% 480) %% 5 + 1]),
    stringsAsFactors = FALSE)
}
