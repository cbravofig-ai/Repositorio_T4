# T4 — Portafolio de visualización con ggplot2

Fundamentos de Programación para Análisis Económico · UdeC-EAN

## Cómo reproducir

1. Abrir `T4.Rproj` en RStudio.
2. Correr `scripts/t4_parte1.R` y luego `scripts/t4_parte2.R` de principio a fin.
   Paquetes: `dplyr`, `tidyr`, `ggplot2`, `scales`, `ggrepel`, `patchwork`.
3. Las figuras quedan en `output/figuras/` (PNG a 300 dpi; la figura compuesta también en PDF).

Datos en `data/raw/`: `casen_reducido.csv`, `salario_minimo_chile.csv` y `ene-2026-06-mjj.csv`.

## Parte 1 — Tres gráficos, tres bases

| Figura | Qué muestra | Hallazgo |
|---|---|---|
| `t4p1_g1_casen.png` | Educación e ingreso por género, una recta por género (CASEN) | La brecha de género en el ingreso se achica con más años de educación |
| `t4p1_g2_salario.png` | Ingreso mínimo nominal y real (pesos de 2010), 2014–2025 | Entre 2014 y 2025, el mínimo nominal subió 135 %, pero el real solo 44 % |
| `t4p1_g3_ene_categoria.png` | Composición por categoría ocupacional según sexo (ENE, ponderado) | Los hombres se concentran más en el empleo privado y las mujeres en el público |
| `t4p1_g3_simulacion_sin_ponderar.png` | La misma composición por categoría y sexo, con y sin `fact_cal` | Sin fact_cal, la brecha en el empleo asalariado se exagera |

## Parte 2 — Portafolio y figura compuesta

| Figura | Qué muestra | Hallazgo |
|---|---|---|
| `t4p2_g1_casen.png` | Scatter de referencia CASEN | Más educación, más ingreso, en hombres y mujeres |
| `t4p2_g2_horas_educacion.png` | Distribución ponderada de las horas habituales por nivel educativo (sin código 9 ni centinelas) | A menor educación, mayor dispersión en las horas de trabajo |
| `t4p2_g3_habituales_efectivas.png` | Horas habituales contra efectivas, con diagonal y = x | Tener empleo no garantiza trabajar: casi 5 % de los ocupados tuvo cero horas efectivas (ausentes) |
| `t4p2_g4_informalidad_region_ic.png` | Informalidad por región con IC al 95 % | La Araucanía y Ñuble lideran las tasas de informalidad laboral |
| `t4p2_g5_informalidad_edad_ic.png` | Informalidad por tramo de edad y sexo con IC al 95 % | La informalidad laboral dibuja una U: castiga a jóvenes y adultos mayores |
| `t4p2_figura_compuesta.png` / `.pdf` | Paneles A-B-C: G4, G2 y G5 | Radiografía del empleo en Chile: territorio, educación y edad |

## Reflexión

**1. ¿Cuál de los cinco gráficos comunica mejor su hallazgo y por qué?**
El Gráfico 2 (distribución de horas habituales por nivel educativo) es el que 
mejor comunica su hallazgo. Al aprovechar el eje horizontal y usar facetas para 
separar los niveles, permite ver la forma real en la que se agrupa la población. 
No esconde los datos detrás de un promedio general engañoso, sino que hace visualmente 
evidente cómo la dispersión de la jornada laboral aumenta a menor escolaridad.

**2. En el gráfico 2, ¿por qué elegiste ese geom y no otro?**
Elegí densidad (geom_density) porque permite comparar visualmente la forma 
completa de las distribuciones sin sobreponerlas, lo que habría saturado y 
dificultado leer un histograma. A diferencia de un boxplot, que resume todo a 
cuartiles cerrados, la densidad revela los puntos de concentración (como la 
jornada legal de 44 horas) y la dispersión real de cada grupo.

**3. ¿Qué agrega la figura compuesta que no dan los gráficos por separado?**
La figura compuesta agrupa los determinantes del empleo (territorio, educación y 
edad) en un solo panel, permitiendo analizar el mercado laboral desde distintos puntos 
de vista y no de manera aislada. Al juntar estos tres gráficos, construimos una 
narrativa visual unificada y completa sobre la estructura de la informalidad y la 
precariedad en el país.

## Declaración de autoría y uso de IA
- Herramienta utilizada: Gemini / Claude
- Para qué la usé: Claude generó una base de los scripts y respuestas modelo, dada 
la instrucción de mi parte de qué quería lograr en los gráficos; también lo usé 
para entender el código y corregir errores.
- Qué hice yo: revisé y entendí cada línea, elegí y escribí los títulos, adapté 
el código, y redacté las respuestas y la reflexión con mis palabras.
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.
