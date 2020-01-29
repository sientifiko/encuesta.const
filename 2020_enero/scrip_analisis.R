
# iniciar librerías
library(tidyverse); library(officer);library(bbplot); library(flextable)


# ============================ NORMALIZAR LA DATA ========================

# importar encuesta
encuesta <- read.csv("2020-enero-final.csv")

# reordernar factores de variables
# region (por si quieren las regiones ordenadas de norte a sur)
# levels(encuesta$region)
# encuesta$region <- factor(encuesta$region,
#                           levels(encuesta$region)[c(3,15,1,4,7,16,14,13,11,12,6,2,9,8,5,10)])

# ordenar escalas likert
for(i in 7:10){
  encuesta[,i] <- factor(encuesta[,i],
                         levels(encuesta[,i])[c(4, 2, 5, 1, 3)]  )
}

# generar un nse como factor
encuesta$nse.factor <- factor(as.factor(encuesta$nse), 
                              labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# centrar en 0 variables de organización política y económica
encuesta$org.pol.cent <- encuesta$org.pol - 5
encuesta$org.econ.cent <- encuesta$org.econ - 5

# centrar en 0 variables de ontología y posicionamiento político
encuesta$realidad.cent <- encuesta$realidad - 5
encuesta$pos.politico.cent <- encuesta$pos.politico - 5


# ======================= DESCRIPCIÓN DE LA MUESTRA ==========================

# por GENERO
genero <- encuesta %>% 
  group_by(genero) %>% 
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

genero_plot <- ggplot(genero, aes(reorder(genero, -perc), perc, fill = genero)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  labs(title = "Porcentaje por género",
       subtitle = paste("N:", sum(genero$n)))


# por REGION
region <- encuesta %>% 
  group_by(region) %>% 
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

region_plot <- ggplot(region, aes(reorder(region, -perc), perc, fill = region)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  labs(title = "Porcentaje por region",
       subtitle = paste("N:", sum(region$n)))


# por NSE
nse <- encuesta %>% 
  group_by(nse.factor) %>% 
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

nse_plot <- ggplot(nse, aes(nse.factor, perc, fill = nse.factor)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  labs(title = "Porcentaje por NSE",
       subtitle = paste("N:", sum(nse$n)))


# por EDAD 
ggplot(encuesta, aes(edad)) +
  bbc_style() +
  geom_density() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  labs(title = "Edad")

# se descubren outliers en EDAD, que procedemos a eliminar
max(encuesta$edad, na.rm = T)
encuesta$edad[encuesta$edad == 2920] <- NA
encuesta$edad[encuesta$edad == 2722] <- NA

min(encuesta$edad, na.rm = T)
encuesta$edad[encuesta$edad == -1] <- NA
encuesta$edad[encuesta$edad == 0] <- NA

edad <- ggplot(encuesta, aes(edad)) +
  bbc_style() +
  geom_density() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  geom_text(aes(label = paste("media:",round(mean(edad, na.rm = T),1), "\n",
                              "d.estandar:", round(sd(edad, na.rm = T),1)), 
                x = 60, y = .06)) +
  labs(title = "Edad")

# por ESCOLARIDAD
escolaridad <- ggplot(encuesta, aes(esc)) +
  bbc_style() +
  geom_density() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  geom_text(aes(label = paste("media:",round(mean(edad, na.rm = T),1), "\n",
                              "d.estandar:", round(sd(edad, na.rm = T),1)), 
                x = 25, y = .15)) +
  labs(title = "Escolaridad")



# == por ORGANIZACIÓN POLÍTICA y ECONÓMICA, y posición política
org_pol_econ <- ggplot(encuesta, aes(org.pol.cent, org.econ.cent)) +
  bbc_style() +
  geom_jitter()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  geom_text(aes(label = "Totalmente Jerarquica", x = 5, y = 0), 
            angle = -90, colour = "red") +
  geom_text(aes(label = "Totalmente Horizontal", x = -5, y = 0), 
            angle = 90, colour = "red") + # eje x
  geom_text(aes(label = "Totalmente Regulada", x = 0, y = 5),colour = "blue") +
  geom_text(aes(label = "Totalmente Desregulada", x = 0, y = -5), colour = "blue") + # eje y
  theme(legend.position = "none") +
  labs(title = "Organización política y económica", 
       subtitle = "Azul: económica, Rojo: política")
  
# == por TENDENCIA POLÍTICA Y NIVEL DE REALISMO

pospol_realismo <- ggplot(encuesta, aes(realidad.cent, pos.politico.cent)) +
  bbc_style() +
  geom_jitter()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  geom_text(aes(label = "Independiente de sujetos que la perciben", x = 5, y = 0), 
            angle = -90, colour = "red") +
  geom_text(aes(label = "Construcción social", x = -5, y = 0), 
            angle = 90, colour = "red") + # eje x
  geom_text(aes(label = "Extrema derecha", x = 0, y = 5),colour = "blue") +
  geom_text(aes(label = "Extrema izquierda", x = 0, y = -5), colour = "blue") + # eje y
  theme(legend.position = "none") +
  labs(title = "Posicionamiento político y percepción ontológica", 
       subtitle = "Azul: político, Rojo: ontológica")







# ================== DESCRIPTIVO VARIABLES PRINCIPALES ============

# por ESCALAS LIKERT
esc.likert <- encuesta %>% 
  select(7:10) %>%
  gather("likert", "valor", 1:4) %>%
  group_by(likert, valor) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))
esc.likert$valor <- factor(as.factor(esc.likert$valor),
                           levels(as.factor(esc.likert$valor))[c(4, 2, 5, 1, 3)])

esc.likert$likert <- as.factor(esc.likert$likert)
levels(esc.likert$likert) <- c("Me siento muy esperanzado por el \ncambio de la constitución",
                               "Creo que el acuerdo para el cambio \nconstitucional fue justo y transparente",
                               "Confío en que una nueva constitución \nnos hará un mejor país a la larga",
                               "Creo que el cambio constitucional es \nnecesario para avanzar en materia de derechos y \njusticia social")

likert <- ggplot(esc.likert, aes(valor, perc, fill = valor)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .7),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  facet_wrap(.~likert, nrow = 2) +
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12))


# por APOYO A NUEVA CONSTITUCIÓN
apoyo <- encuesta %>%
  group_by(nueva.const) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

ggplot(apoyo, aes(reorder(nueva.const, -perc), perc, fill = nueva.const)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  labs(title = "Porcentaje por cambio de constitución",
       subtitle = paste("N:", sum(nse$n)))

# por MECANISMO
mecanismo <- encuesta %>%
  group_by(mecanismo) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

ggplot(mecanismo, aes(reorder(mecanismo, -perc), perc, fill = mecanismo)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  labs(title = "Porcentaje por mecanismo de nueva constitución",
       subtitle = paste("N:", sum(nse$n)))

table(encuesta$nueva.const, encuesta$mecanismo)

# ============================ RELACIONES ========================





# ========================  EXPORTANDO TODO A PPT ==================


read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(genero_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(region_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(nse_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(edad, location = ph_location_left()) %>% 
  ph_with_gg(escolaridad, location = ph_location_right()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(org_pol_econ, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(pospol_realismo, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  # desde aquí gráficas sobre preguntas clave
  ph_with_gg(likert, location = ph_location_fullsize()) %>% 
  print(target = "2020 enero.pptx")


# esto es para agregar tablas si se desea
# read_pptx() %>%
#   add_slide("Title and Content", "Office Theme") %>%
#   ph_with(flextable(data.frame), 
#           location = ph_location_template(left = x, top = y)) %>%
#   print(target = "nombre_archivo.pptx")

