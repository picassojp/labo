# rm(list=ls())
# library(dplyr)
# 
# exp <- "" #str de 2 digitos
# model_nmbr <- "" #equivale a i en ZZ
# model_name <- "" #equivale a iteracion_bayesiana en ZZ
# 
# base_path <- '~/buckets/b1/exp/colectivos/EC'
# #base_path <- '/home/user/projects/dmeyf_R/exp/colectivos/EC' #local
# 
# setwd(paste0(base_path,exp,'-6-results'))
# 
# cortes <- seq(from=7000, to= 15000, by= 250)
# tb_prediccion <- read.csv(paste0("pred_",model_nmbr,"_",model_name,".csv"), sep="\t")
# df_true <- read.csv("../../../datasets/202107.csv")
# df_all <- full_join(tb_prediccion,df_true,by="numero_de_cliente" ) %>% arrange(desc(prob))
# 
# ganancias <- data.frame(corte=numeric(),ganancia=numeric())
# for (corte in cortes){
#   ganancia <- (df_all %>%
#     mutate(Predicted = case_when(row_number() <= corte ~ 1, TRUE ~ 0 )) %>%
#     mutate(ganancia = case_when(Predicted == 1 & true_class == 1 ~ 78000,
#                                 Predicted == 1 & true_class == 0 ~ -2000,
#                                 TRUE ~ 0)) %>%
#       summarise(GANANCIA = round(sum(ganancia)/1000000,digits=5)))[1,"GANANCIA"]
#   
#   ganancias <- ganancias %>% add_row(corte=corte,ganancia=ganancia)
# }
# ganancias <- ganancias %>% mutate(modelo = paste0(exp,"_",model_nmbr,'_',model_name))
# 
# write.csv(ganancias,paste0('ganancias_',exp,"_",model_nmbr,'_',model_name,'.csv'),row.names=FALSE)


##lo mismo pero en data.table

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#cargo los paquetes que necesito
require("data.table")

#Parametros del script

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "AD0" #str de 3 digitos
PARAM$model_nmbr  <- "01" #equivale a i en ZZ
PARAM$model_name  <- "108" #equivale a iteracion_bayesiana en ZZ

PARAM$exp_input  <- "ZZ9420AD0"

# FIN Parametros del script

exp <- "" 
model_nmbr <- "" 
model_name <- "" 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/colectivos/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/colectivos/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

cortes <- seq(from=7000, to= 15000, by= 250)

tb_prediccion <- fread(paste0(base_dir,"exp/colectivos/",PARAM$exp_input,"/pred_",PARAM$model_nmbr,"_",PARAM$model_name,".csv"))
df_true <- fread(paste0( base_dir, "exp/colectivos/202107.csv"))


df_all <- df_true[tb_prediccion,on='numero_de_cliente']
setorder( df_all, -prob )

ganancias=data.table(corte=numeric(), ganancia=numeric())
for (corte in cortes){
  
  df_all[  , Predicted := 0L ]
  df_all[ 1:corte, Predicted := 1L ]
  df_all[, ganancia := 78000 * (Predicted == 1 & true_class == 1) + (-2000) * (Predicted == 1 & true_class == 0)]
  ganancia <- round(colSums(df_all[,'ganancia']) / 1000000, digits=5)
  new_row    <- data.table("corte" = corte, "ganancia" = ganancia)
  ganancias <- rbindlist(list(ganancias, new_row))
}
ganancias[,modelo := paste0(exp,"_",model_nmbr,'_',model_name)]

fwrite(  ganancias,
         file= paste0('ganancias_',PARAM$experimento,"_",PARAM$model_nmbr,'_',PARAM$model_name,'.csv'),
         sep= "," )