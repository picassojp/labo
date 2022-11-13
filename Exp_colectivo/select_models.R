rm(list=ls())

library(dplyr)

base_path <- "/home/user/projects/dmeyf_R/exp/colectivos/"
setwd(base_path)
list.dirs()

todos=data.table(corte=numeric(), ganancia=numeric(),modelo=character())
for (dir in list.dirs(full.names=TRUE,recursive=FALSE)){
  todos_exp <- list.files(path = dir,
                         pattern = "*.csv", full.names = TRUE) %>% 
    lapply(read.csv) %>%
    bind_rows
  todos <- bind_rows(todos,todos_exp)
}

mejores <- todos %>%
  group_by(modelo) %>%
  filter(ganancia == max(ganancia)) %>%
  arrange(modelo,ganancia)

write.csv(mejores,paste0('mejores.csv'),row.names=FALSE)