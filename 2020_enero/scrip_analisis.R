
# chequear si packages est�n instalados
paquetes <- c("tidyverse", "officer", "bbplot", "flextable")
nuevos.paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos.paquetes)>0){install.packages(nuevos.paquetes)}
rm(paquetes, nuevos.paquetes)

# iniciar packages
library(tidyverse); library(officer);library(bbplot); library(flextable)
options(scipen = 999)

  
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
rm(i)
# generar un nse como factor
encuesta$nse.factor <- factor(as.factor(encuesta$nse), 
                              labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# centrar en 0 variables de organizaci�n pol�tica y econ�mica
encuesta$org.pol.cent <- encuesta$org.pol - 5
encuesta$org.econ.cent <- encuesta$org.econ - 5

# centrar en 0 variables de ontolog�a y posicionamiento pol�tico
encuesta$realidad.cent <- encuesta$realidad - 5
encuesta$pos.politico.cent <- encuesta$pos.politico - 5


# ======================= DESCRIPCI�N DE LA MUESTRA ==========================

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
  labs(title = "Porcentaje por g�nero",
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



# == por ORGANIZACI�N POL�TICA y ECON�MICA, y posici�n pol�tica
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
  labs(title = "Organizaci�n pol�tica y econ�mica", 
       subtitle = "Azul: econ�mica, Rojo: pol�tica")
  
# == por TENDENCIA POL�TICA Y NIVEL DE REALISMO

pospol_realismo_plot <- ggplot(encuesta, aes(realidad.cent, pos.politico.cent)) +
  bbc_style() +
  geom_jitter()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  geom_text(aes(label = "Independiente de sujetos que la perciben", x = 5, y = 0), 
            angle = -90, colour = "red") +
  geom_text(aes(label = "Construcci�n social", x = -5, y = 0), 
            angle = 90, colour = "red") + # eje x
  geom_text(aes(label = "Extrema derecha", x = 0, y = 5),colour = "blue") +
  geom_text(aes(label = "Extrema izquierda", x = 0, y = -5), colour = "blue") + # eje y
  theme(legend.position = "none") +
  labs(title = "Posicionamiento pol�tico y percepci�n ontol�gica de realidad", 
       subtitle = "Azul: pol�tico, Rojo: ontol�gica")


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
levels(esc.likert$likert) <- c("Me siento muy esperanzado por el \ncambio de la constituci�n",
                               "Creo que el acuerdo para el cambio \nconstitucional fue justo y transparente",
                               "Conf�o en que una nueva constituci�n \nnos har� un mejor pa�s a la larga",
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


# por APOYO A NUEVA CONSTITUCI�N
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
  labs(title = "Porcentaje por cambio de constituci�n",
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
  labs(title = "Porcentaje por mecanismo de nueva \nconstituci�n",
       subtitle = paste("N:", sum(nse$n)))

table(encuesta$nueva.const, encuesta$mecanismo)

# ============================ RELACIONES ========================
paquetes <- c("MASS", "stargazer", "corrplot", "Hmisc", "ggpubr")
nuevos.paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos.paquetes)>0){install.packages(nuevos.paquetes)}
rm(paquetes, nuevos.paquetes)

library(MASS); library(stargazer); library(corrplot); library(Hmisc); library(ggpubr)

# Usaremos una regresi�n ordinal para predecir niveles de desconfianza o
# escepticismo respecto del acuerdo o el cambio constitucional

# Tomando como base las gr�ficas descriptivas, identificamos que las variables
# con suficiente variabilidad para actuar como predictores, corresponden a
# "nse", "esc","realidad", "org.pol", "org.econ" y "pos.politico" 

# breve exploraci�n gr�fica

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
# lo que me interesa predecir es la desconfianza, as� que reducir� a 3 variables
# (desconfianza, neutral y confianza) e invertir� relaci�n

encuesta$esperanza.reduc <- case_when(
  encuesta$esperanza == "Muy de acuerdo" | encuesta$esperanza == "De acuerdo" ~ "De acuerdo",
  encuesta$esperanza == "Neutral" ~ "Neutral",
  encuesta$esperanza == "Muy en desacuerdo" | encuesta$esperanza == "En desacuerdo" ~ "Desacuerdo"
) %>% factor(c("De acuerdo", "Neutral", "Desacuerdo"))

encuesta$eval.acuerdo.reduc <- case_when(
  encuesta$eval.acuerdo == "Muy de acuerdo" | encuesta$eval.acuerdo == "De acuerdo" ~ "De acuerdo",
  encuesta$eval.acuerdo == "Neutral" ~ "Neutral",
  encuesta$eval.acuerdo == "Muy en desacuerdo" | encuesta$eval.acuerdo == "En desacuerdo" ~ "Desacuerdo"
) %>% factor(c("De acuerdo", "Neutral", "Desacuerdo"))
  

# las discretizar� para sacar correlaciones
encuesta$esperanza.reduc.num <- encuesta$esperanza.reduc %>% as.numeric()
encuesta$eval.acuerdo.reduc.num <- encuesta$eval.acuerdo.reduc %>% as.numeric()

# exploremos la relaci�n en posicionamiento pol�tico, para identificar colinealidades
# u otros
cormat_pol <- rcorr(as.matrix(encuesta[, c(5,6,11:14, 24, 25)]), type = "spearman")

corrplot(cormat_pol$r, type = "upper", order = "hclust", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, p.mat = cormat_pol$P, sig.level = 0.5,
         insig = "blank", diag = F, tl.cex = .5, tl.pos = "td")


# hay correlaci�n en algunas variables, pero efecto es bastante peque�o, as� que procedemos
# de igual forma. Tambi�n omitiremos algunos supuestos sobre predictores categ�ricos
# y los trataremos como variables cont�nuas por simplicidad 

# estandarizaci�n de variables a z-score
encuesta$realidad.std <- scale(encuesta$realidad)
encuesta$pos.politico.std <- scale(encuesta$pos.politico)
encuesta$org.econ.std <- scale(encuesta$org.econ)
encuesta$org.pol.std <- scale(encuesta$org.pol)

# primer modelo, predecir "me siento muy esperanzado por cambio en constituci�n"
esp.model <- polr(esperanza.reduc ~ nse + esc + 
                    realidad.std + pos.politico.std + org.econ.std + org.pol.std, 
                  data = encuesta, Hess = T)

# resumen general de modelo
tabla_coef_esperanza <- coef(summary(esp.model))
t.valores_esperanza <- coef(summary(esp.model))[, "t value"]
p.valores_esperanza <- pnorm(abs(t.valores_esperanza), lower.tail = F) * 2

cbind(tabla_coef_esperanza, "p.valor" = p.valores_esperanza)

# mostrando resultados con stargazer 
stargazer(esp.model, type = "text")

# tabla de OR
OR.esperanza <- as.data.frame(cbind(OR = exp(coef(esp.model)),confint(esp.model))) %>% 
  rownames_to_column()

# flextable de OR.esperanza para exportar
OR.esperanza.table <- flextable(OR.esperanza, cwidth = 1.5, cheight = .5) %>% 
  align(align = "center", part = "all") %>% bold(part = "header") %>%
  color(i = with(OR.esperanza, sign(`2.5 %`)==sign(`97.5 %`)),
        color = "green")

# plot de modelo 1
modelo1 <- ggplot(encuesta, aes(esperanza.reduc,org.econ, colour = pos.politico)) +
  theme_bw() +
  geom_boxplot(outlier.colour = "white")+
  geom_jitter()+
  scale_color_gradient(low = "red", high = "yellow") +
  geom_text(aes(label = "Total regulaci�n", x = 2, y = 10.5),colour = "blue") +
  geom_text(aes(label = "Total desregulaci�n", x = 2, y = 0.5), colour = "blue") + 
  scale_y_continuous(breaks = seq(1,10,1)) +
  theme(text = element_text(size = 20)) +
  labs(x = "Hay esperanzas en el cambio de constituci�n", 
       y = "Organizaci�n econ�mica",
       colour = "Posicionamiento pol�tico\nderecha = 10\nizquierda = 1")

# primer modelo, predecir "Creo que el acuerdo para el cambio constitucional fue justo y transparente"
acuerdo.model <- polr(eval.acuerdo.reduc ~ nse + esc + 
                    realidad.std + pos.politico.std + org.econ.std + org.pol.std, 
                  data = encuesta, Hess = T)

# resumen general de modelo
tabla_coef_acuerdo <- coef(summary(acuerdo.model))
t.valores_acuerdo <- coef(summary(acuerdo.model))[, "t value"]
p.valores_acuerdo <- pnorm(abs(t.valores_acuerdo), lower.tail = F) * 2

cbind(tabla_coef_acuerdo, "p.valor" = p.valores_acuerdo)

# mostrando resultados con stargazer 
stargazer(acuerdo.model, type = "text")

# tabla de OR
OR.acuerdo <- as.data.frame(cbind(OR = exp(coef(acuerdo.model)),
                                  confint(acuerdo.model))) %>% 
  rownames_to_column()

# flextable de OR.esperanza para exportar
OR.acuerdo.table <- flextable(OR.acuerdo, cwidth = 1.5, cheight = .5) %>% 
  align(align = "center", part = "all") %>% bold(part = "header") %>%
  color(i = with(OR.acuerdo, sign(`2.5 %`)==sign(`97.5 %`)),
        color = "green")

# plot de modelo 2
modelo2 <- ggplot(encuesta, aes(eval.acuerdo.reduc, pos.politico)) +
  theme_bw() +
  geom_boxplot(outlier.colour = "white")+
  geom_jitter()+
  geom_text(aes(label = "Derecha", x = 2, y = 10.5),colour = "blue") +
  geom_text(aes(label = "Izquierda", x = 2, y = 0.5), colour = "blue") + 
  scale_y_continuous(breaks = seq(1,10,1)) +
  theme(text = element_text(size = 20)) +
  labs(x = "Fue acuerdo para cambio de constituci�n transparente y justo", 
       y = "Posicionamiento pol�tico")


# modelo para analizar percepci�n de evaluaci�n del acuerdo y la esperanza que genera
modelo3 <- polr(esperanza.reduc ~ nse + esc + 
                  pos.politico.std + scale(eval.acuerdo.reduc.num), 
     data = encuesta, Hess = T) 

# resumen general de modelo
tabla_coef <- coef(summary(modelo3))
t.valores <- coef(summary(modelo3))[, "t value"]
p.valores <- pnorm(abs(t.valores), lower.tail = F) * 2

cbind(tabla_coef, "p.valor" = p.valores)

# mostrando resultados con stargazer 
stargazer(modelo3, type = "text")

# tabla de OR
OR.set <- as.data.frame(cbind(OR = exp(coef(modelo3)),
                                  confint(modelo3))) %>% 
  rownames_to_column()


# flextable de OR.esperanza para exportar
OR.table <- flextable(OR.set, cwidth = 1.5, cheight = .5) %>% 
  align(align = "center", part = "all") %>% bold(part = "header") %>%
  color(i = with(OR.set, sign(`2.5 %`)==sign(`97.5 %`)),
        color = "green")

# tabla auxiliar
df.relacion <- encuesta %>% 
  group_by(eval.acuerdo.reduc, esperanza.reduc) %>%
  dplyr::summarize(n= dplyr::n()) %>%
  mutate(perc= n/sum(n))
  

# plot de modelo 3
modelo3 <- ggplot(encuesta, aes(esperanza.reduc, pos.politico)) +
  theme_bw() +
  geom_boxplot()+
  geom_jitter() +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, vjust = .9, hjust = 1)) +
  geom_text(aes(label = "Derecha", x = 2, y = 10.5),colour = "blue") +
  geom_text(aes(label = "Izquierda", x = 2, y = 0.5), colour = "blue") + 
  facet_wrap(.~ eval.acuerdo.reduc) +
  labs(x = "Hay esperanza por el cambio de constituci�n",
       y = "Posicionamiento pol�tico", 
       title = "Fue acuerdo para cambio de constituci�n transparente y justo")


# ========================  EXPORTANDO TODO A PPT ==================


read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Descripci�n de muestra", location = ph_location_type("title")) %>%
  
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
  
  # desde aqu� gr�ficas sobre preguntas clave
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Preguntas clave sobre proceso", location = ph_location_type("title")) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(likert_plot, location = ph_location_fullsize()) %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(apoyo_plot, location = ph_location_fullsize()) %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_gg(mecanismo_plot, location = ph_location_fullsize()) %>% 
  
  # desde aqu� tablas y gr�ficas de relaciones
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Correlaciones", location = ph_location_type("title")) %>%
  
  add_slide("Title and Content", "Office Theme") %>%
  ph_with(value = "Confianza en esperanza por cambio de constituci�n", 
          location = ph_location_type("title")) %>%
  ph_with(OR.esperanza.table,
          location = ph_location_template(left = 2, top = 3, width = 40,
                                          height = 50)) %>%
  
  add_slide("Title and Content", "Office Theme") %>%
  ph_with_gg(modelo1, location = ph_location_fullsize()) %>% 
  
  add_slide("Title and Content", "Office Theme") %>%
  ph_with(value = "Confianza en transparencia y justicia de acuerdo para cambio de constituci�n", 
          location = ph_location_type("title")) %>%
  ph_with(OR.acuerdo.table,
          location = ph_location_template(left = 2, top = 3, width = 40,
                                          height = 50)) %>%
  
  add_slide("Title and Content", "Office Theme") %>%
  ph_with_gg(modelo2, location = ph_location_fullsize()) %>% 
  
  add_slide("Title and Content", "Office Theme") %>%
  ph_with(value = "Esperanza por cambio de constituci�n y confianza sobre transparencia de proceso", 
          location = ph_location_type(type = "title")) %>%
  ph_with(OR.table,
          location = ph_location_template(left = 2, top = 3, width = 40,
                                          height = 50)) %>%
  
  add_slide("Title and Content", "Office Theme") %>%
  ph_with_gg(modelo3, location = ph_location_fullsize()) %>% 
  
  print(target = "2020_enero/2020 enero.pptx")









