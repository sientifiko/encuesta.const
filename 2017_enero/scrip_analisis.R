
# iniciar librerías
library(tidyverse); library(officer); library(bbplot)


# ============================ NORMALIZAR LA DATA ========================

# importar encuesta
encuesta <- read.csv("2017-enero-final.csv")

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
encuesta$nse.factor <- factor(encuesta$nse, 
                              labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))


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


# exportando gráficas a pptx
read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(genero_plot) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(region_plot) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(nse_plot) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(likert) %>% 
  print(target = "2017 enero.pptx")




# ============================ RELACIONES ========================








