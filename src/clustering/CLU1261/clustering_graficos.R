#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")
require("ggplot2")
require("corrplot")
require("gtools")

setwd( "C:/Users/juancho/Desktop/DMEF/" ) 


theme <- theme(text = element_text(size=10),
               plot.title = element_text(size = 12, face = "bold.italic", hjust = 0.5), 
               axis.title.x = element_text(size = 10, face="bold", colour='black'),         
               axis.title.y = element_text(size = 10, face="bold"),
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), 
               legend.title = element_text(face="bold"))

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
dataset  <- fread( "./labo/src/clustering/CLU1261/cluster_de_bajas.csv", stringsAsFactors= TRUE)

summary(dataset)

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")

# Graficamos el dataset y el modelo
N<-length(campos_buenos)
n<-2
combinaciones <- combinations(N, n, campos_buenos)
head(combinaciones) # Las primeras combinaciones

plot(x = dataset[,ccomisiones_otras], y = dataset[,ctrx_quarter], col=dataset[,cluster2])
ggplot(data=dataset, aes(x=cpayroll_trx,y = ccomisiones_otras, color=factor(cluster2)))+geom_point()
ggplot(data=dataset, aes(x=get(combinaciones[4,][2]),y = get(combinaciones[4,][1]), color=factor(cluster2)))+geom_point()+
  xlab(combinaciones[4,][2]) +
  ylab(combinaciones[4,][1])

#donde voy a grabar todo
pdf( "cluster_iteraciones.pdf")

for(c in 1:length(combinaciones)){ 
  print(c)
  ggplot(data=dataset, aes(x=get(combinaciones[c,][2]),y = get(combinaciones[c,][1]), color=factor(cluster2)))+geom_point()+
    xlab(combinaciones[c,][2]) +
    ylab(combinaciones[c,][1])
  
}

#termino la impresion
dev.off()

pdf( "cluster_iteraciones.pdf")

for(c in 1:length(combinaciones)){ 
  print(c)
  plot( x= dataset[,get(combinaciones[c,][2])],
      y= dataset[,get(combinaciones[c,][1])],
      xlab= combinaciones[c,][2],
      ylab= combinaciones[c,][1],
      col= dataset[,cluster2],
      #type= "l",
  )
}
#termino la impresion
dev.off()



qplot(factor(cluster2), y = ctrx_quarter, data = dataset, geom = "boxplot", xlab="cluster2")

