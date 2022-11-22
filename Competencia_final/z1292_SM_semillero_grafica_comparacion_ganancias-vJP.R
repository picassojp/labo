# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_ganancias_semillerio"
PARAM$exp_input <- "ZZ9420KF0_semillerio"

PARAM$corte <- 11500 # cantidad de envios
# FIN Parametros del script

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

#base_dir <- "./labo/Competencia_final/" #para correr localmente


# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/comp_final/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/comp_final/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

#path_experimento_semillerio <- paste0(base_dir, "exp/comp_final/", PARAM$exp_input)
path_experimento_semillerio <- paste0(base_dir, PARAM$exp_input)

archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

# Esto es MUY dependiente del formato del nombre de los experimentos, se puede romper muy facil
ksemillas <- strtoi(sapply(strsplit(archivos, "_"), "[", 3))

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "datasets/competenciaFINAL_2022.csv.gz")
#arch_dataset <- paste0("./datasets/competenciaFINAL_2022.csv.gz") #para correr localmente
dataset <- fread(arch_dataset)

dataset_sept <- dataset[foto_mes == 202109]
rm(dataset)

# Nos quedamos con las 2 columnas que nos resultan relevantes
dataset_sept <- dataset_sept[, .(numero_de_cliente)]

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset_sept[, numero_de_cliente])
tb_prediccion_semillerio_acumulado <- data.table(numero_de_cliente = dataset_sept[, numero_de_cliente])

for (archivo in archivos) {
  
  ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 3))
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(paste0(path_experimento_semillerio, "/", archivo))
  setorder(tb_prediccion, numero_de_cliente)
  setorder(tb_ranking_semillerio, numero_de_cliente)
  setorder(tb_prediccion_semillerio_acumulado, numero_de_cliente)
  
  # repara bug en z1292, si se fixea ahi, esto no genera problemas
  tb_prediccion[, rank := frank(-prob, ties.method = "random")]
  
  # Generamos predicción del semillerio
  tb_ranking_semillerio[, paste0("rank_", ksemilla) := tb_prediccion$rank]
  
  tb_prediccion_semillerio_acumulado[, paste0("prediccion_ind_", ksemilla) := tb_prediccion$rank]
  
  #if (ncol(tb_prediccion_semillerio_acumulado) == 2) {
  #    # Esta es la predicción del semillerio para la semilla i-esima
  #    tb_prediccion_semillerio <- data.table(
  #        tb_ranking_semillerio[, list(numero_de_cliente)],
  #        # prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  #        prediccion = tb_prediccion$rank
  #    )
  #} else {
  #    # Esta es la predicción del semillerio para la semilla i-esima
  #    tb_prediccion_semillerio <- data.table(
  #        tb_ranking_semillerio[, list(numero_de_cliente)],
  #        # prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  #        (
  #            tb_prediccion_semillerio_acumulado[, ncol(tb_prediccion_semillerio_acumulado) - 1, with = FALSE] +
  #            tb_prediccion_semillerio_acumulado[, ncol(tb_prediccion_semillerio_acumulado), with = FALSE]
  #        ) / 2 # excluye el numero_de_cliente del cálculo de la media
  #    )
  #}
  #colnames(tb_prediccion_semillerio) <- c("numero_de_cliente", "prediccion")
  #tb_prediccion_semillerio_acumulado[, paste0("prediccion_acc_", ksemilla) := tb_prediccion$rank]
  
  # Generamos predicción individual
  setorder(tb_prediccion, -prob)
  tb_prediccion[, Predicted := 0]
  tb_prediccion[1:PARAM$corte, Predicted := 1L]
  
  # Esta es la predicción del semillerio para la semilla i-esima
  tb_prediccion_semillerio <- data.table(
    tb_ranking_semillerio[, list(numero_de_cliente)],
    prediccion = round(apply(tb_ranking_semillerio[, c(-1)], 1, median, na.rm=T), digits = 0) # excluye el numero_de_cliente del cálculo de la media
    #prediccion = round(rowMeans(tb_ranking_semillerio[, c(-1)]), digits = 0) # excluye el numero_de_cliente del cálculo de la media
    )
  tb_prediccion_semillerio_acumulado[, paste0("prediccion_acc_", ksemilla) := tb_prediccion_semillerio$prediccion]
  setorder(tb_prediccion_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor
  tb_prediccion_semillerio[, Predicted := 0]
  tb_prediccion_semillerio[1:PARAM$corte, Predicted := 1L]
  
}

nom_rank  <- paste0( "rank_",
                     PARAM$exp_input,
                     ".csv"  )

nom_pred  <- paste0( "pred_",
                     PARAM$exp_input,
                     ".csv"  )

fwrite(tb_prediccion_semillerio_acumulado, file=nom_rank, sep = ",")
fwrite(tb_prediccion_semillerio[,c(-2)], file=nom_pred, sep = ",")

# pdf("semillerio_vs_individuales.pdf")
# secuencia <- seq(from = 1, to = length(tb_ganancias$semilla))
# yminimo <- min(tb_ganancias$individual) - 0.005 * min(tb_ganancias$individual)
# ymaximo <- max(tb_ganancias$individual) + 0.005 * max(tb_ganancias$individual)
# plot(secuencia, tb_ganancias$semillerio,
#      type = "l",
#      col = "red",
#      ylim = c(yminimo, ymaximo),
#      xlab = "Semillas",
#      ylab = "Ganancia total Julio 2021",
#      main = "Experimento Semillerio - 11000 envios"
# )
# points(secuencia, tb_ganancias$individual, col = "blue")
# abline(h=mean(tb_ganancias$individual), col = "green")
# legend("bottomleft",
#        inset = .05,
#        c("Ensemble Semillerio", "Semillas sueltas", "Media semillas sueltas"),
#        fill = c("red", "blue", "green"),
#        horiz = FALSE
# )
# dev.off()