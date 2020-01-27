
# iniciar librerías
library(tidyverse)


# ============================ LIMPIEZA GENERAL ========================

# importar encuesta
encuesta <- read.csv("2017-enero-final.csv")

# reordernar factores de variables
# region
encuesta$region <- factor(encuesta$region,
                          levels(encuesta$region)[c(3,15,1,4,7,16,14,13,11,6,2,9,8,5,10)])
# escalas likert
for(i in 7:10){
  encuesta[,i] <- factor(encuesta[,i],
                         levels(encuesta[,i])[c(4, 2, 5, 1, 3)]  )
}

# ======================= DESCRIPCIÓN DE LA MUESTRA ==========================






# ============================ RELACIONES ========================








