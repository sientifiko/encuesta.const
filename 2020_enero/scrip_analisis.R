
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
                         levels(encuesta[,i])[c(4, 2, 5, 1, 3)])
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

edad_plot <- ggplot(encuesta, aes(edad)) +
  bbc_style() +
  geom_density() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  geom_text(aes(label = paste("media:",round(mean(edad, na.rm = T),1), "\n",
                              "d.estandar:", round(sd(edad, na.rm = T),1)), 
                x = 60, y = .06)) +
  labs(title = "Edad")

# por ESCOLARIDAD
escolaridad_plot <- ggplot(encuesta, aes(esc)) +
  bbc_style() +
  geom_density() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  geom_text(aes(label = paste("media:",round(mean(edad, na.rm = T),1), "\n",
                              "d.estandar:", round(sd(edad, na.rm = T),1)), 
                x = 25, y = .15)) +
  labs(title = "Escolaridad")



# == por ORGANIZACIÓN POLÍTICA y ECONÓMICA, y posición política
org_pol_econ_plot <- ggplot(encuesta, aes(org.pol.cent, org.econ.cent)) +
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

pospol_realismo_plot <- ggplot(encuesta, aes(realidad.cent, pos.politico.cent)) +
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
  labs(title = "Posicionamiento político y percepción ontológica de realidad", 
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

likert_plot <- ggplot(esc.likert, aes(valor, perc, fill = valor)) +
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

apoyo_plot <- ggplot(apoyo, aes(reorder(nueva.const, -perc), perc, fill = nueva.const)) +
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

mecanismo_plot <- ggplot(mecanismo, aes(reorder(mecanismo, -perc), perc, fill = mecanismo)) +
  bbc_style() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  geom_text(aes(label = scales::percent(perc))) +
  labs(title = "Porcentaje por mecanismo de nueva \nconstitución",
       subtitle = paste("N:", sum(nse$n)))

table(encuesta$nueva.const, encuesta$mecanismo)

# ============================ RELACIONES ========================
library(MASS); library(stargazer); library(corrplot); library(Hmisc); library(ggpubr)

# Usaremos una regresión ordinal para predecir niveles de desconfianza o
# escepticismo respecto del acuerdo o el cambio constitucional

# Tomando como base las gráficas descriptivas, identificamos que las variables
# con suficiente variabilidad para actuar como predictores, corresponden a
# "nse", "esc","realidad", "org.pol", "org.econ" y "pos.politico" 

# breve exploración gráfica

ggarrange(ggplot(encuesta, aes(realidad)) +
            geom_density(),
          ggplot(encuesta, aes(pos.politico)) +
            geom_density(),
          ggplot(encuesta, aes(org.econ)) +
            geom_density(),
          ggplot(encuesta, aes(org.pol)) +
            geom_density(),
          nrow = 2, ncol = 2)

# Respecto a las VD, corresonden a las variables "esperanza" y "eval.acuerdo"
# lo que me interesa predecir es la desconfianza, así que reduciré a 3 variables
# (desconfianza, neutral y confianza) e invertiré relación

encuesta$esperanza.reduc <- case_when(
  encuesta$esperanza == "Muy de acuerdo" | encuesta$esperanza == "De acuerdo" ~ "Confianza",
  encuesta$esperanza == "Neutral" ~ "Neutral",
  encuesta$esperanza == "Muy en desacuerdo" | encuesta$esperanza == "En desacuerdo" ~ "Desconfianza"
) %>% factor(c("Confianza", "Neutral", "Desconfianza"))


encuesta$esperanza.rev <- fct_rev(encuesta$esperanza)
encuesta$eval.acuerdo.rev <- fct_rev(encuesta$eval.acuerdo)



# las discretizaré para sacar correlaciones
encuesta$esperanza.rev.num <- encuesta$esperanza.rev %>% as.numeric()
encuesta$eval.acuerdo.rev.num <- encuesta$eval.acuerdo.rev %>% as.numeric()

# exploremos la relación en posicionamiento político, para identificar colinealidades
# u otros
cormat_pol <- rcorr(as.matrix(encuesta[, c(5,6,11:14, 24, 25)]), type = "spearman")

corrplot(cormat_pol$r, type = "upper", order = "hclust", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, p.mat = cormat_pol$P, sig.level = 0.5,
         insig = "blank", diag = F, tl.cex = .7, tl.pos = "td")


# hay correlación en algunas variables, pero efecto es bastante pequeño, así que procedemos
# de igual forma. También omitiremos algunos supuestos sobre predictores categóricos
# y los trataremos como variables contínuas por simplicidad 

# estandarización de variables a z-score
encuesta$realidad.std <- scale(encuesta$realidad)
encuesta$pos.politico.std <- scale(encuesta$pos.politico)
encuesta$org.econ.std <- scale(encuesta$org.econ)
encuesta$org.pol.std <- scale(encuesta$org.pol)

# primer modelo, predecir "me siento muy esperanzado por cambio en constitución"
esp.model <- polr(esperanza.rev ~ nse + esc + 
                    realidad.std + pos.politico.std + org.econ.std + org.pol.std, 
                  data = encuesta, Hess = T)

# resumen general de modelo
tabla_coef_esperanza <- coef(summary(esp.model))
t.valores_esperanza <- coef(summary(esp.model))[, "t value"]
p.valores_esperanza <- pnorm(abs(t.valores_esperanza), lower.tail = F) * 2

cbind(tabla_coef_esperanza, "p.valor" = p.valores_esperanza)


# mostrando resultados con stargazer con stargazer
stargazer(esp.model, type = "text")

# tabla de OR
OR.esperanza <- as.data.frame(cbind(OR = exp(coef(esp.model)),confint(esp.model))) %>% 
  rownames_to_column()

# flextable de OR.esperanza para exportar
OR.esperanza.table <- flextable(OR.esperanza, cwidth = 1.5, cheight = 1) %>% 
  align(align = "center", part = "all") %>% bold(part = "header") %>%
  color(i = with(OR.esperanza, sign(`2.5 %`)==sign(`97.5 %`)),
        color = "green")


ggplot(encuesta, aes(org.pol.std, org.econ.std, shape = as.factor(esperanza.rev))) +
  theme_bw() +
  geom_jitter()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  # scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  # scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  geom_text(aes(label = "Jerarquica", x = max(org.pol.std)+.5, y = 0), 
            angle = -90, colour = "red") +
  geom_text(aes(label = "Horizontal", x = min(org.pol.std)-.5, y = 0), 
            angle = 90, colour = "red") + # eje x
  geom_text(aes(label = "Regulada", x = 0, y = max(org.econ.std)+.5),colour = "blue") +
  geom_text(aes(label = "Desregulada", x = 0, y = min(org.econ.std)-.5), colour = "blue") + # eje y
  theme(legend.position = "right") +
  labs(x = "Organización política", y = "Organización económica",
       shape = "Me siento muy esperanzado \npor cambio en constitución")




# ========================  EXPORTANDO TODO A PPT ==================


read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(genero_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(region_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(nse_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(edad_plot, location = ph_location_left()) %>% 
  ph_with_gg(escolaridad_plot, location = ph_location_right()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(org_pol_econ_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(pospol_realismo_plot, location = ph_location_fullsize()) %>% 
  
  # desde aquí gráficas sobre preguntas clave
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(likert_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(apoyo_plot, location = ph_location_fullsize()) %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(mecanismo_plot, location = ph_location_fullsize()) %>% 
  
  # desde aquí tablas y gráficas de relaciones
  add_slide("Title and Content", "Office Theme") %>%
  ph_with(OR.esperanza.table,
          location = ph_location_template(left = 2, top = .5, width = 50,
                                          height = 50)) %>%
  
  print(target = "2020 enero.pptx")



# esto es para agregar tablas si se desea
# read_pptx() %>%
#   add_slide("Title and Content", "Office Theme") %>%
#   ph_with(flextable(data.frame), 
#           location = ph_location_template(left = x, top = y)) %>%
#   print(target = "nombre_archivo.pptx")

