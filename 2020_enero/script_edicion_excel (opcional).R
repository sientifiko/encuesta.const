
# abrir archivo en csv
encuesta <- read.delim("enero-2020.csv", sep = ";")

# cambiar el nombre de las columnas
colnames(encuesta) <- c("esperanza", "necesario", "eval.acuerdo", "mejor.pais", "nueva.const",
                        "mecanismo", "genero", "region", "comuna", "edad", "nse", "org.pol", 
                        "realidad", "org.econ", "pos.politico", "esc")

# reorganizar las columnas en un orden más cómodo
encuesta <- encuesta[, c(7:11, 16, 1:4, 12, 14, 15, 13, 5, 6)]

# cambiar nombre de regiones
levels(encuesta$region) <- c("Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Araucania",
                             "RM", "Valparaiso", "Ohiggins", "Maule", "Biobio", "Los Lagos",
                             "Aysen", "Magallanes", "Los Rios", "Arica y Parinacota", "Ñuble")

# cambiar etiquetas de variable nueva.const
levels(encuesta$nueva.const) <- c("No votara", "Aprueba", "Nulo o Blanco", "Rechaza")

# cambiar las etiquetas de variable mecanismo
levels(encuesta$mecanismo) <- c("No votara", "CC", "CM", "Nulo o Blanco")

# exportar a csv versión final
write.csv(encuesta, "2020-enero-final.csv", row.names = F)




