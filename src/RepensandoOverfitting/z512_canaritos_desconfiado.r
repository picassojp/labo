#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/juancho/Desktop/DMEF/")  #Establezco el Working Directory
# Poner sus semillas
semillas <- c(668111, 945577, 433889, 914371, 676241)

#cargo el dataset
dataset  <- fread( "./datasets/competencia1_2022.csv")

#uso esta semilla para los canaritos
set.seed(semillas[1])

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 13:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
                 data= dataset[ foto_mes==202101 ,],
                 model= TRUE,
                 xval= 0,
                 cp= -1,
                 minsplit= 200,
                 minbucket= 100,
                 maxdepth= 10)

#este árbol no clasifica en baja, sólo en continua
pdf(file = "./work/arbol_canaritos_desconfiado.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

