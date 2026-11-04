# =============================================================================
# T4 — Parte 1: Tres gráficos, tres bases
#
# Autor: Camila Bravo Figueroa
# Fecha: 03-11-2026
#
# Pregunta: ¿qué cuentan la CASEN, el ingreso mínimo y la ENE cuando se
#   grafican de forma honesta?
# =============================================================================


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

azul <- "#223c6a"; oro <- "#e69b0a"; gris <- "grey60"

# -----------------------------------------------------------------------------
# TEMA PROPIO — como función, activado con theme_set()
# -----------------------------------------------------------------------------
tema_t4 <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title            = element_text(face = "bold", color = azul),
          panel.grid.minor      = element_blank(),                            
          plot.title.position   = "plot",                                     
          plot.caption          = element_text(color = "grey40", size = base * 0.75, hjust = 0),
          plot.caption.position = "plot",
          legend.position       = "top",
          strip.text            = element_text(face = "bold"),
          plot.background       = element_rect(fill = "white", color = NA),   
          plot.margin           = margin(10, 15, 10, 10))                    
}
theme_set(tema_t4())          # desde aquí, TODOS los gráficos usan el tema

# Formato chileno para los ejes
pesos <- label_currency(big.mark = ".", decimal.mark = ",")

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# GRÁFICO 1 · CASEN: educación e ingreso por género
# -----------------------------------------------------------------------------
casen <- read.csv("data/raw/casen_reducido.csv") |>
  filter(!is.na(ingreso))                 # 60 personas − 5 NA = 55

nrow(casen)

# Pendiente de cada recta: cuánto sube el ingreso por año de educación
pendientes <- casen |>
  group_by(genero) |>
  summarise(n         = n(),
            pendiente = coef(lm(ingreso ~ educ))[2],
            ing_medio = mean(ingreso))
pendientes

# Dónde termina cada recta (para poner la etiqueta directa al final)
fin_rectas <- casen |>
  group_by(genero) |>
  summarise(ingreso = coef(lm(ingreso ~ educ))[1] +           # intercepto
              coef(lm(ingreso ~ educ))[2] * max(educ), # + pendiente × máx
            educ    = max(educ)) |>
  mutate(etiqueta = ifelse(genero == "F", "Mujeres", "Hombres"))

# HERENCIA DE aes(): color va en el ggplot() GLOBAL, por eso lo heredan los
# puntos Y geom_smooth(). geom_smooth() agrupa por color y ajusta UNA recta por
# género. Si color fuera solo dentro de geom_point(), saldría UNA sola recta.
g1 <- ggplot(casen, aes(educ, ingreso, color = genero)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_text(data = fin_rectas, aes(label = etiqueta),
            hjust = -0.1, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(F = oro, M = azul), guide = "none") +
  scale_y_continuous(labels = pesos) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(title    = "La brecha de género en el ingreso disminuye con más años de educación",
       x = "Años de educación", y = "Ingreso mensual",
       caption = "Fuente: CASEN, muestra del curso (55 personas con ingreso, sin ponderar).")
g1

ggsave("output/figuras/t4p1_g1_casen.png", g1, width = 8, height = 5.5, dpi = 300)

# PREGUNTA: ¿qué pregunta responde este gráfico que no respondería la versión
# con una sola recta?
# RESPUESTA :
# Con una sola recta solo veríamos que estudiar más años sirve para ganar más 
# en general. Al separarlo con dos rectas, el gráfico responde si ese beneficio 
# económico es igual para hombres y mujeres. Esto nos deja ver que a las mujeres 
# les sube más el ingreso por cada año extra de estudio en esta muestra ($46.862 
# por año frente a $38.295 en los hombres), haciendo que la brecha a favor de 
# los hombres se achique a mayor escolaridad (pasando de unos $92.700 con 8 años 
# a unos $24.200 con 16 años). Como se trata de solo 55 personas sin ponderar, 
# el gráfico describe únicamente a esta muestra y no se puede generalizar a Chile.


# -----------------------------------------------------------------------------
# GRÁFICO 2 · Ingreso mínimo: nominal contra real (pesos de 2010)
# -----------------------------------------------------------------------------
salario <- read.csv("data/raw/salario_minimo_chile.csv") |>
  mutate(nominal = salario_minimo,
         real    = salario_minimo / ipc * 100)    # IPC base 2010 = 100

# Formato LARGO: una fila por año y serie, una columna "serie" para mapear
salario_largo <- salario |>
  pivot_longer(cols = c(nominal, real),
               names_to = "serie", values_to = "pesos") |>
  mutate(serie = recode(serie, nominal = "Nominal",
                        real    = "Real (pesos de 2010)"))

salario_largo |> filter(anio %in% c(2014, 2025))

# Crecimiento 2014–2025 de cada serie (para el título y la respuesta)
salario |>
  summarise(crec_nominal = last(nominal) / first(nominal) - 1,
            crec_real    = last(real)    / first(real)    - 1)

g2 <- ggplot(salario_largo, aes(anio, pesos, color = serie, linetype = serie)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  # Etiquetas DIRECTAS al final de cada línea (reemplazan la leyenda)
  geom_text(data = filter(salario_largo, anio == max(anio)),
            aes(label = serie), hjust = -0.08, fontface = "bold") +
  scale_color_manual(values = c("Nominal" = gris, "Real (pesos de 2010)" = azul)) +
  scale_linetype_manual(values = c("Nominal" = "dashed", "Real (pesos de 2010)" = "solid")) +
  scale_y_continuous(labels = pesos) +
  scale_x_continuous(breaks = seq(2014, 2025, 2),
                     expand = expansion(mult = c(0.02, 0.35))) +
  labs(title    = "Entre 2014 y 2025, el mínimo nominal subió 135 %, pero el real solo 44 %",
       x = NULL, y = "Ingreso mínimo mensual",
       caption = "Fuente: ingreso mínimo legal e IPC (base 2010 = 100), 2014–2025. Real = nominal / IPC × 100.") +
  coord_cartesian(clip = "off") +          # que la etiqueta no se corte
  theme(legend.position = "none")          # sin leyenda: etiquetas directas
g2

ggsave("output/figuras/t4p1_g2_salario.png", g2, width = 8, height = 5.5, dpi = 300)

# PREGUNTAS: ¿por qué aquí el eje Y no necesita partir en 0 y en un gráfico de
# barras sí? ¿Qué ganas usando color Y tipo de línea?
# RESPUESTA: 
# En los gráficos de barras leemos el valor por la longitud, por lo que no partir 
# de cero exagera las diferencias visuales. En cambio, en las líneas nos fijamos en 
# la trayectoria y las pendientes, así que acortar el eje Y funciona como un zoom 
# para notar mejor la distancia entre las series sin distorsionar los datos. 
# Además, usar colores junto con distintos tipos de línea asegura que el gráfico 
# se entienda bien al imprimirlo en blanco y negro o si lo lee una persona con 
# daltonismo. Finalmente, las etiquetas directas reemplazan la necesidad de usar 
# una leyenda, haciendo mucho más directo el hallazgo de que entre 2014 y 2025 
# el mínimo nominal subió un 135 %, pero el real lo hizo solo un 44 %.

# -----------------------------------------------------------------------------
# GRÁFICO 3 · ENE: en qué trabajan hombres y mujeres
# -----------------------------------------------------------------------------
ocupados <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1) |>                                    # solo ocupados
  mutate(sexo = ifelse(sexo == 1, "Hombres", "Mujeres"),
         categoria = case_when(categoria_ocupacion == 1     ~ "Empleador",
                               categoria_ocupacion == 2     ~ "Cuenta propia",
                               categoria_ocupacion == 3     ~ "Asalariado privado",
                               categoria_ocupacion == 4     ~ "Asalariado público",
                               categoria_ocupacion %in% 5:6 ~ "Servicio doméstico",
                               categoria_ocupacion == 7     ~ "Familiar no remunerado"),
         categoria = factor(categoria, levels = c("Asalariado privado", "Asalariado público",
                                                  "Cuenta propia", "Empleador",
                                                  "Servicio doméstico", "Familiar no remunerado")))

nrow(ocupados)    # 41.850 ocupados en la muestra

# Composición PONDERADA: % de cada categoría dentro de cada sexo
comp <- ocupados |>
  group_by(sexo, categoria) |>
  summarise(pob = sum(fact_cal), .groups = "drop_last") |>
  mutate(pct = pob / sum(pob)) |>
  ungroup()

# Diferencia mujeres − hombres en puntos porcentuales
dif <- comp |>
  select(sexo, categoria, pct) |>
  pivot_wider(names_from = sexo, values_from = pct) |>
  mutate(dif_pp = 100 * (Mujeres - Hombres)) |>
  arrange(desc(abs(dif_pp)))
dif

# Lo mismo SIN ponderar
ocupados |>
  count(sexo, categoria) |>
  group_by(sexo) |>
  mutate(pct_sin_ponderar = n / sum(n)) |>
  left_join(select(comp, sexo, categoria, pct_ponderado = pct),
            by = c("sexo", "categoria"))

# Paleta Okabe-Ito (accesible para daltonismo, S9)
okabe <- c(azul, "#56B4E9", oro, "#009E73", "#CC79A7", gris)   # Okabe-Ito con el azul y oro del curso

g3 <- ggplot(ocupados, aes(y = sexo, fill = categoria, weight = fact_cal)) +
  geom_bar(position = position_fill(reverse = TRUE), width = 0.6) +
  # Valores escritos (solo los segmentos de 4 % o más, para que se lean)
  geom_text(data = filter(comp, pct >= 0.04),
            aes(x = pct, y = sexo, label = label_percent(accuracy = 1)(pct)),
            position = position_fill(vjust = 0.5, reverse = TRUE),
            inherit.aes = FALSE, color = "white", fontface = "bold", size = 3.5) +
  scale_x_continuous(labels = label_percent(), expand = c(0, 0)) +   # desde 0
  scale_fill_manual(values = okabe) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title    = "Los hombres se concentran más en el empleo privado y las mujeres en el público",
       x = "% de los ocupados de cada sexo", y = NULL, fill = NULL,
       caption = "Fuente: INE, Encuesta Nacional de Empleo, trimestre mayo-julio 2026. Ocupados, ponderado con fact_cal.") +
  theme(panel.grid = element_blank())
g3

ggsave("output/figuras/t4p1_g3_ene_categoria.png", g3, width = 8, height = 5.5, dpi = 300)

# PREGUNTAS: ¿en qué categoría está la mayor diferencia entre hombres y
# mujeres, y en cuál casi no hay? Da las cifras. ¿Qué cambiaría si borras
# weight = fact_cal?
# RESPUESTA:
# La mayor diferencia está en los asalariados privados, donde hay más hombres 
# (64,9 %) que mujeres (53,9 %) con una brecha de 10,9 puntos. En el sector 
# público se invierte, con más mujeres (17 %) que hombres (9,3 %), mientras que 
# en cuenta propia son casi idénticos (21,1 % contra 21,2 %). Si borramos el 
# ponderador (weight = fact_cal), el gráfico mostraría solo a la muestra; al 
# comparar los datos sin ponderar, la brecha privada sube a 12,5 puntos, la pública 
# crece de 7,8 a 9,2 puntos y la de cuenta propia cambia de signo, lo que demuestra 
# que es un error usar esos números para sacar conclusiones a nivel nacional.

# Para comprobar se simula lo siguiente

# Calculamos la misma composición dos veces con y sin el factor de expansión.
comparacion <- bind_rows(
  ocupados |> count(sexo, categoria, wt = fact_cal) |> mutate(version = "Ponderado (fact_cal)"),
  ocupados |> count(sexo, categoria)                |> mutate(version = "Sin ponderar")
) |>
  group_by(version, sexo) |>
  mutate(pct = n / sum(n)) |>          # % de cada categoría dentro de cada sexo
  ungroup()

# Brecha hombres − mujeres (en puntos porcentuales) en cada versión
brechas <- comparacion |>
  select(version, sexo, categoria, pct) |>
  pivot_wider(names_from = sexo, values_from = pct) |>
  mutate(brecha_pp = round(100 * (Hombres - Mujeres), 1)) |>
  select(version, categoria, brecha_pp) |>
  pivot_wider(names_from = version, values_from = brecha_pp)
brechas

# Las dos versiones una sobre otra (facetas) para verlo
g3_sim <- ggplot(comparacion, aes(x = pct, y = sexo, fill = categoria)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.6) +
  facet_wrap(~ version, ncol = 1) +
  scale_x_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(values = okabe) +
  labs(title = "Sin fact_cal, la brecha en el empleo asalariado se exagera",
       x = "% de los ocupados de cada sexo", y = NULL, fill = NULL,
       caption = "Fuente: INE, ENE mayo-julio 2026. Ocupados; panel superior ponderado, inferior sin ponderar.") +
  theme(panel.grid = element_blank())
g3_sim

ggsave("output/figuras/t4p1_g3_simulacion_sin_ponderar.png", g3_sim, width = 8, height = 5.5, dpi = 300)

