# =============================================================================
# T4 — Parte 2: Portafolio y figura compuesta
#
# Autor: Camila Bravo Figueroa
# Fecha: 03-11-2026
#
# Pregunta general: ¿cómo se distribuyen el ingreso (CASEN), las horas y la
#   informalidad laboral (ENE) en Chile, y qué gráfico lo comunica mejor?
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(patchwork)

azul <- "#223c6a"; oro <- "#e69b0a"; gris <- "grey70"
pesos <- label_currency(big.mark = ".", decimal.mark = ",")
pct   <- label_percent(accuracy = 1, decimal.mark = ",")

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# TEMA PROPIO — como función, con dos (o más) cambios, activado con theme_set()
# -----------------------------------------------------------------------------
tema_t4 <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title         = element_text(face = "bold", color = azul),  # cambio 1
          panel.grid.minor   = element_blank(),                            # cambio 2
          plot.title.position = "plot",                                    # cambio 3
          plot.caption       = element_text(color = "grey40", size = base * 0.75),
          plot.caption.position = "plot")
}
theme_set(tema_t4())          # desde aquí, TODOS los gráficos usan el tema


# -----------------------------------------------------------------------------
# DATOS
# -----------------------------------------------------------------------------
casen <- read.csv("data/raw/casen_reducido.csv") |> filter(!is.na(ingreso))

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

ocupados <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1) |>                                  # solo ocupados
  mutate(informal   = ocup_form == 2,                    # 2 = informal
         mujer      = sexo == 2,
         sexo_txt   = ifelse(mujer, "Mujeres", "Hombres"),
         region_txt = regiones[region],
         # Centinelas 888 (no sabe) y 999 (no responde) → NA (manual INE)
         habituales = na_if(na_if(habituales, 888), 999),
         efectivas  = na_if(na_if(efectivas,  888), 999))

nrow(ocupados)                                  # 41.850
sum(is.na(ocupados$habituales))                 # 111 centinelas en habituales
sum(is.na(ocupados$efectivas))                  # centinelas en efectivas

fuente_ene <- "Fuente: INE, ENE mayo-julio 2026. Ocupados, ponderado con fact_cal."


# -----------------------------------------------------------------------------
# GRÁFICO 1 · Scatter CASEN (referencia de estilo)
# -----------------------------------------------------------------------------
g1 <- ggplot(casen, aes(educ, ingreso, color = genero)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  scale_color_manual(values = c(F = oro, M = azul),
                     labels = c(F = "Mujeres", M = "Hombres")) +
  scale_y_continuous(labels = pesos) +
  labs(title = "Más educación, más ingreso, en hombres y mujeres",
       x = "Años de educación", y = "Ingreso mensual", color = NULL,
       caption = "Fuente: CASEN, muestra del curso (55 personas, sin ponderar).") +
  theme(legend.position = "top")
g1
ggsave("output/figuras/t4p2_g1_casen.png", g1, width = 8, height = 5, dpi = 300)


# -----------------------------------------------------------------------------
# GRÁFICO 2 · Distribución de las horas habituales por nivel educativo
# -----------------------------------------------------------------------------
table(ocupados$cine11_1d)

horas_educ <- ocupados |>
  filter(cine11_1d != 9, !is.na(habituales)) |>         # sin código 9 ni centinelas
  mutate(nivel = case_when(cine11_1d == 0     ~ "Menos que primaria",
                           cine11_1d == 1     ~ "Primaria",
                           cine11_1d == 2     ~ "Secundaria",
                           cine11_1d == 3     ~ "Terciaria corta",
                           cine11_1d == 4     ~ "Universitaria",
                           cine11_1d %in% 5:6 ~ "Posgrado"),
         nivel = factor(nivel, levels = c("Menos que primaria", "Primaria", "Secundaria",
                                          "Terciaria corta", "Universitaria", "Posgrado")))

mediana_pond <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

resumen_horas <- horas_educ |>
  group_by(nivel) |>
  summarise(n       = n(),
            mediana = mediana_pond(habituales, fact_cal),
            media   = weighted.mean(habituales, fact_cal),
            pct_parcial = weighted.mean(habituales <= 30, fact_cal))  
resumen_horas

horas_educ <- horas_educ |>
  group_by(nivel) |>
  mutate(w = fact_cal / sum(fact_cal)) |>
  ungroup()

g2 <- ggplot(horas_educ, aes(habituales)) +
  geom_density(aes(weight = w), fill = azul, color = NA, alpha = 0.8, bw = 2) +
  geom_vline(xintercept = 44, linetype = "dashed", color = "grey40") +
  geom_vline(data = resumen_horas, aes(xintercept = media), color = oro, linewidth = 0.8) +
  facet_wrap(~ nivel, ncol = 1, strip.position = "left") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  coord_cartesian(xlim = c(0, 90)) +           
  labs(title    = "A menor educación, mayor dispersión en las horas de trabajo",
       subtitle = "Densidad ponderada por nivel · línea dorada: media · línea punteada: jornada legal de 44 h",
       x = "Horas habituales por semana", y = NULL,
       caption = paste(fuente_ene, "Sin centinelas 888/999 ni nivel 'sin clasificación' (código 9).")) +
  theme(axis.text.y = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        panel.grid.major.y = element_blank())
g2
ggsave("output/figuras/t4p2_g2_horas_educacion.png", g2, width = 9, height = 7, dpi = 300)

# JUSTIFICACIÓN DE GEOM:
# Usar densidad en facetas permite comparar visualmente la forma completa de las distribuciones por 
# nivel educativo sin sobreponerlas (lo que saturaría un histograma). A diferencia de un boxplot, la densidad 
# revela los picos exactos de concentración (como la jornada legal de 44 horas) y la dispersión real de la población.


# -----------------------------------------------------------------------------
# GRÁFICO 3 · Horas habituales contra efectivas
# -----------------------------------------------------------------------------
horas2 <- ocupados |> filter(!is.na(habituales), !is.na(efectivas))  

ceros <- horas2 |>
  summarise(n_ceros   = sum(efectivas == 0),
            pct_ceros = weighted.mean(efectivas == 0, fact_cal),
            pct_menos = weighted.mean(efectivas < habituales, fact_cal),
            pct_igual = weighted.mean(efectivas == habituales, fact_cal))
ceros

g3 <- ggplot(horas2, aes(habituales, efectivas)) +
  geom_bin_2d(aes(weight = fact_cal), binwidth = c(2, 2)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey20") +
  annotate("text", x = 104, y = 92, label = "efectivas = habituales",
           hjust = 0, size = 3.2) +
  annotate("label", x = 90, y = 18, hjust = 0, size = 3.2, label.size = 0,
           label = "Fila de ceros: ocupados ausentes\n(vacaciones, licencia, permiso)") +
  scale_fill_viridis_c(trans = "log10",
                       labels = label_number(big.mark = ".", decimal.mark = ",")) +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 120)) +
  labs(title    = "Tener empleo no garantiza trabajar",
       x = "Horas habituales por semana", y = "Horas efectivas la semana pasada",
       fill = "Ocupados\n(escala log)",
       caption = paste(fuente_ene, "Sin centinelas 888/999 en ninguna de las dos variables."))
g3
ggsave("output/figuras/t4p2_g3_habituales_efectivas.png", g3, width = 8, height = 6.5, dpi = 300)


# -----------------------------------------------------------------------------
# GRÁFICO 4 · Informalidad por región con su IC al 95 %
# -----------------------------------------------------------------------------
ic_region <- ocupados |>
  group_by(region_txt) |>
  summarise(n    = n(),
            p    = weighted.mean(informal, fact_cal),
            se   = sqrt(p * (1 - p) / n),
            t    = qt(0.975, df = n - 1),
            bajo = p - t * se,
            alto = p + t * se)

inf_nacional <- weighted.mean(ocupados$informal, ocupados$fact_cal)
inf_nacional                                         # ≈ 26,5 %

ic_region |> arrange(desc(p))
ic_region |> filter(bajo > inf_nacional)

g4 <- ggplot(ic_region, aes(p, reorder(region_txt, p))) +
  geom_vline(xintercept = inf_nacional, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(xmin = bajo, xmax = alto), color = azul, size = 0.3) +
  annotate("text", x = inf_nacional, y = 0.5, label = "Promedio nacional",
           hjust = -0.05, size = 3.2, color = "grey30") +   
  scale_x_continuous(labels = pct) +
  labs(title = "La Araucanía y Ñuble lideran las tasas de informalidad laboral",
       x = "% de ocupados informales (IC 95 %)", y = NULL,
       caption = paste(fuente_ene, "IC aproximado: no considera el diseño muestral."))
g4
ggsave("output/figuras/t4p2_g4_informalidad_region_ic.png", g4, width = 8, height = 6, dpi = 300)

# LIMITACIÓN: ¿por qué los IC verdaderos serían más anchos?
# RESPUESTA: Los intervalos calculados asumen la fórmula básica de un muestreo 100 % al azar. Como la ENE agrupa a las personas 
# por zonas geográficas (estratos y conglomerados) y quienes viven cerca suelen tener condiciones similares, el margen de error 
# verdadero es mayor (efecto de diseño). Esto hace que los intervalos reales deban ser más anchos, por lo que algunas regiones 
# que aquí parecen alejarse del promedio nacional podrían no estarlo estadísticamente.


# -----------------------------------------------------------------------------
# GRÁFICO 5 · TU PREGUNTA: ¿la informalidad dibuja una U con la edad?
# -----------------------------------------------------------------------------
ic_edad <- ocupados |>
  filter(edad >= 15) |>
  mutate(tramo = cut(edad, breaks = c(14, 24, 34, 44, 54, 64, Inf),
                     labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))) |>
  group_by(tramo, sexo_txt) |>
  summarise(n    = n(),
            p    = weighted.mean(informal, fact_cal),
            se   = sqrt(p * (1 - p) / n),
            t    = qt(0.975, df = n - 1),
            bajo = p - t * se,
            alto = p + t * se, .groups = "drop")
ic_edad

g5 <- ggplot(ic_edad, aes(tramo, p, color = sexo_txt, group = sexo_txt)) +
  geom_line(linewidth = 0.6, alpha = 0.5) +
  geom_pointrange(aes(ymin = bajo, ymax = alto), size = 0.3,
                  position = position_dodge(width = 0.3)) +
  geom_text(data = filter(ic_edad, tramo == "65+"),
            aes(label = sexo_txt), hjust = -0.3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(Hombres = azul, Mujeres = oro), guide = "none") +
  scale_y_continuous(labels = pct, limits = c(0, NA)) +
  scale_x_discrete(expand = expansion(add = c(0.5, 1))) +
  labs(title = "La informalidad laboral dibuja una U: castiga a jóvenes y adultos mayores",
       x = "Tramo de edad", y = "% de ocupados informales (IC 95 %)",
       caption = paste(fuente_ene, "IC aproximado: no considera el diseño muestral."))
g5
ggsave("output/figuras/t4p2_g5_informalidad_edad_ic.png", g5, width = 8, height = 5, dpi = 300)

# LIMITACIÓN de este gráfico:
# RESPUESTA: Al basarse en datos de un solo trimestre (corte transversal), es solo una "foto" del momento. Compara a 
# jóvenes y mayores de hoy, pero no demuestra la trayectoria vital de una persona (no prueba que alguien se vuelva más 
# informal al envejecer), ya que las diferencias observadas pueden ser simplemente generacionales (efecto de cohorte).


# -----------------------------------------------------------------------------
# FIGURA COMPUESTA — tres paneles con patchwork
# -----------------------------------------------------------------------------
figura <- ((g4 | (g2 / g5)) & labs(caption = NULL)) +    
  plot_layout(widths = c(1, 1.2)) +
  plot_annotation(title      = "Radiografía del empleo en Chile: cómo el territorio, la educación y la edad determinan la informalidad",
                  caption    = "Fuente: INE, ENE mayo-julio 2026. Ocupados, ponderado con fact_cal. IC al 95 % aproximados.",
                  tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", color = azul)))
figura

ggsave("output/figuras/t4p2_figura_compuesta.png", figura, width = 16, height = 10, dpi = 300)
ggsave("output/figuras/t4p2_figura_compuesta.pdf", figura, width = 16, height = 10)   # vectorial
