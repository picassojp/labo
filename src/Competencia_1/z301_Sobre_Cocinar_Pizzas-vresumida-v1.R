##
## Sobre como cocinar Pizzas
#VERSION RESUMIDA PARA HACER LA OB
#-v1: se modificó la clase binaria (incluye baja +1 y +2). 
# Se agregó la probabilidad de corte como un argumento más.
# Se trabajó con todas las variables
# se agregó una parte para analizar la ganancia en cada hoja de los parámetros de OB
# se modificaron las funciones de ganancia poniendo la clase ternaria
# se acotaron los HP en función al PrimerModelo-v5-R (con mejor público en kaggle)
# se cambiaron las iteraciones de 150 a 100
# se agregó la probabilidad de corte como un HP más a optimizar
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Success is a lousy teacher. It seduces smart people into thinking they can't 
## lose.
## --- Bill Gates

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")
require("rpart.plot")

# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/juancho/Desktop/DMEF/")
# Poner sus semillas
semillas <- c(668111, 945577, 433889, 914371, 676241)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]
# # Creamos una clase binaria
# dataset[, clase_binaria := ifelse(
#   clase_ternaria == "BAJA+2",
#   "evento",
#   "noevento"
# )]


# Clases BAJAS+1 y BAJA+2 combinadas
dataset[, clase_binaria := ifelse(
  clase_ternaria == "CONTINUA",
  "noevento",
  "evento"
)]



# # Borramos el target viejo
# dataset[, clase_ternaria := NULL]

# #(tomado de z401_Sobre_Campos-v2):
# # Supongamos que tenemos una lista de variables a las que queremos transformar
# mis_variables <- c("ctrx_quarter",
#                    "mprestamos_personales",
#                    "mcuentas_saldo",
#                    "mactivos_margen",
#                    "active_quarter",
#                    "cprestamos_personales",
#                    "cdescubierto_preacordado")
# 
# # A todas las vamos a rankear
# 
# prefix <- "r_"
# for (var in mis_variables) {
#   dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
# }

#guarda los nombres de las variables para pegarlos directamente en el fórmula
#campos <- paste(mis_variables_2, collapse = " + ")
#formula <- paste0( "clase_binaria ~ ", campos )


#funcion de ganancia para la clase binaria
ganancia <- function(probabilidades, clase, prbc) {
  return(sum(
    #(probabilidades >= prbc) * ifelse(clase == "evento", 78000, -2000))
    (probabilidades >= prbc) * ifelse(clase == "BAJA+2", 78000, -2000))
  )
}

# Armamos una función para modelar con el fin de simplificar el código futuro
#para la predicción utiliza la ganancia y la clase binaria
modelo_rpart_ganancia <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10, prbc2=0.025) {
  modelo <- rpart(clase_binaria ~ .-clase_ternaria, data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  #ganancia(test_prediccion[, "evento"], test$clase_binaria, prbc = 0.025) / 0.3
  ganancia(test_prediccion[, "evento"], test$clase_ternaria, prbc = prbc2) / 0.3
}

# Una función auxiliar para los experimentos
experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10, prbc3 = 0.025) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    #train_sample <- tomar_muestra(train)
    r <- modelo_rpart_ganancia(train, test, 
                               cp = cp, ms = ms, mb = mb, md = md, prbc2 = prbc3)
    gan <- c(gan, r)
  }
  mean(gan)
}

## ---------------------------
## Step 11: Mas opt. Bayesiana para 3 parámetros
## ---------------------------

set.seed(semillas[1])
obj_fun_md_ms_mb <- function(x) {
  experimento_rpart_completo(dataset, semillas
                             , md = x$maxdepth
                             , ms = x$minsplit
                             , mb = floor(x$minsplit*x$minbucket)
                             , prbc3 = x$prbc)
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 3L, upper = 7L),
    makeIntegerParam("minsplit",  lower = 1000L, upper = 2000L),
    makeNumericParam("minbucket",  lower = 0L, upper = 1L),
    makeNumericParam("prbc", lower = 0.045, upper = 0.060)
    # makeNumericParam <- para parámetros continuos
  ),
  noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
#ctrl <- setMBOControlTermination(ctrl, iters = 100L)
ctrl <- setMBOControlTermination(ctrl, iters = 50L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch"
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  #opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)

 
# Recommended parameters:
#   maxdepth=6; minsplit=1141; minbucket=0.0724; prbc=0.0482
# Objective: y = 21400000.000 #mejoró!!

# Recommended parameters:
#   maxdepth=5; minsplit=1060; minbucket=0.0885
# Objective: y = 19284000.000

# Recommended parameters:
#   maxdepth=5; minsplit=445; minbucket=0.0825
# Objective: y = 19700000.000

# Recommended parameters:
#   maxdepth=5; minsplit=1998; minbucket=0.175
# Objective: y = 19977333.333

# maxdepth=5; minsplit=174; minbucket=0.884
# Objective: y = 19565333.333

#se prueban los mejores parámetros de esta OB
ms <-1141
mb <- ms*0.0724
md <-6
prbc3 <-0.0482
experimento_rpart_completo(dataset, semillas, cp = -1, ms = ms, mb = mb, md = md, prbc3 = prbc3) 

#se establecen las particiones en train y test
set.seed(semillas[1])
in_training <- caret::createDataPartition(dataset$clase_binaria, p = 0.70,
                                          list = FALSE)
train  <-  dataset[in_training, ]
test   <-  dataset[-in_training, ]

#se entrena un modelo con los parámetros de OB
modelo <- rpart(clase_binaria ~ .-clase_ternaria, data = train,
                xval = 0,
                cp = -1,
                minsplit = ms,
                minbucket = mb,
                maxdepth = md)

# La siguiente función devuelve todas las hojas (nodos terminales) en una tabla
# sobre un modelo. Es una "mejora" sobre la función dada en el script z201:
# - El target ahora debe ser binario: evento/noevento
# - Calcula la ganancia en base
# - Ordena de mayor a menor ganancia
# - Agrega un prefijo para poder juntar diferentes resultados.

leaves_table <- function(model, train, target, prefix = "") {
  leaves_train_table <- data.table(
    # Devuelve en que hoja cae un caso
    leaves = rpart.predict.leaves(model, train, type = "where"),
    classes = train[, clase_ternaria],
    target = train[, get(target)]
  )
  leaves <- dcast(
    leaves_train_table,
    leaves ~ classes, length,
    value.var = "target")
  leaves <- leaves[
    dcast(
      leaves_train_table,
      leaves ~ target, length,
      value.var = "target"),
    on = .(leaves)]
  leaves[, n := evento + noevento]
  leaves[, p := round(evento / n,4)]
  leaves <- leaves[order(-p),]
  leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
  leaves[, ':='(evento = NULL, noevento = NULL)]
  setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
           new = c(paste0(prefix, "b1"),
                   paste0(prefix, "b2"),
                   paste0(prefix, "cont"),
                   paste0(prefix, "n"),
                   paste0(prefix, "p"),
                   paste0(prefix, "gan")))
  leaves[]
}

# Examinamos las nuevas hojas de nuestro modelo para entender las nuevas
# probabilidad. Primero sobre TRAIN
train_binaria <- leaves_table(modelo, train, "clase_binaria")
print(train_binaria)

##se procede con la predicción

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(clase_binaria ~ .-clase_ternaria, data = dataset, #se entrena con todos los datos
                 xval = 0,
                 cp = -1,
                 minsplit = ms,
                 minbucket = mb,
                 maxdepth = md)    #profundidad maxima del arbol

#grafico el arbol
pdf(file = "./work/modelo-090922.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

#reviso las importancias de las variables
print(modelo$variable.importance)
#las variables principales según este modelo:

#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"]]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > prbc3 ) ] #submission Kaggle K101_000
#dapply[ , Predicted := as.numeric( prob_baja2 > prbc3*1.05 ) ] #submission Kaggle K101_001
#dapply[ , Predicted := as.numeric( prob_baja2 > prbc3*1.15 ) ] #submission Kaggle K101_002
#dapply[ , Predicted := as.numeric( prob_baja2 > prbc3*1.10 ) ] #submission Kaggle K101_003

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
#dir.create( "./exp/" )
#dir.create( "./exp/KA2004" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2004/K101_003.csv",
        sep=  "," )

