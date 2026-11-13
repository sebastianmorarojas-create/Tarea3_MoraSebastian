# ============================================================
# 00_generar_base.R  ·  CA0204 · Tarea 3
# NO se edita. Se llama desde SU script cod/01_datos.R así:
#   carne <- "C12345"
#   source("base/00_generar_base.R")
# Genera, dentro de SU proyecto:
#   data/transacciones_2025.csv   (1,1 a 1,3 millones de filas)
#   data/cantones.csv             (catálogo)
#   config/parametros_tarea3.csv  (sus parámetros)
# ============================================================

if (!exists("carne") || carne == "C00000")
  stop('Defina su carné antes del source(): carne <- "C12345"')
if (!file.exists("base/funciones_base.R"))
  stop("No encuentro base/. Abra SU .Rproj y clone el repo base en la carpeta base/.")
for (d in c("data", "config"))
  if (!dir.exists(d)) stop("Primero cree la carpeta ", d, "/ en la raíz de su proyecto.")

source("base/funciones_base.R")
cantones <- read.csv("base/cantones.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

tx <- generar_transacciones(carne, cantones)
tx$fecha <- format(tx$fecha, "%Y-%m-%d")
write.table(tx, "data/transacciones_2025.csv", sep = ";", row.names = FALSE,
            quote = FALSE, fileEncoding = "UTF-8")
file.copy("base/cantones.csv", "data/cantones.csv", overwrite = TRUE)

p <- parametros.tarea3(carne)
write.table(p, "config/parametros_tarea3.csv", sep = ";", row.names = FALSE,
            quote = FALSE, fileEncoding = "UTF-8")

cat("Listo. Filas generadas:", format(nrow(tx), big.mark = " "), "\n")
print(p, row.names = FALSE)
cat("Antes del próximo commit, revise qué debe quedar fuera de Git.\n")
