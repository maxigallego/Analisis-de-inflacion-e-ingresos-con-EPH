# --- INSTALACIÓN (correr solo una vez) ----------------------------------------

library(tidyverse)
library(readxl)
library(scales)
library(ggplot2)

setwd("C:/TP1 - Métodos Cuantitativos/Datos") 

##INFLACION-----------------------------------------------------------------------

# Parámetros globales de exportación
ANCHO  <- 10   # pulgadas
ALTO   <- 6
RES    <- 180  # dpi

# Paleta de colores consistente para los tres países
COLORES <- c("Colombia" = "#1f77b4", "México" = "#ff7f0e", "Perú" = "#2ca02c")


# CARGA Y PROCESAMIENTO

infla_raw <- read_excel("Datos_Infla.xlsx") |>
  mutate(fecha = as.Date(paste(anio, mes, "01", sep = "-")))

# Inflación interanual: variación respecto al mismo mes del año anterior
infla <- infla_raw |>
  arrange(pais, fecha) |>
  group_by(pais) |>
  mutate(
    inflacion = (ipc - lag(ipc, 12)) / lag(ipc, 12) * 100,
    periodo = case_when(
      anio >= 1990 & anio <= 1999 ~ "1. Los 90s (Alta Inflación)",
      anio >= 2000 & anio <= 2009 ~ "2. Los 2000s (Estabilización)",
      anio >= 2010               ~ "3. Post-2010 (Metas de Inflación)"
    )
  ) |>
  ungroup() |>
  filter(!is.na(inflacion))

m2_raw <- read_excel("Datos_M2.xlsx") |>
  mutate(fecha = as.Date(paste(anio, mes, "01", sep = "-")))

m2 <- m2_raw |>
  arrange(pais, fecha) |>
  group_by(pais) |>
  mutate(var_m2 = (m2 - lag(m2, 1)) / lag(m2, 1) * 100) |>
  ungroup() |>
  filter(!is.na(var_m2))

# Merge inflación + M2
merged <- inner_join(
  infla |> select(pais, anio, mes, fecha, inflacion, periodo),
  m2    |> select(pais, fecha, var_m2),
  by = c("pais", "fecha")
)

cat("Merge completo:", nrow(merged), "observaciones\n")
cat("Cobertura por país:\n")
merged |> group_by(pais) |> summarise(desde = min(anio), hasta = max(anio), n = n()) |> print()



# PARTE 1-B: GRÁFICOS DE LÍNEA


# --- G1: Serie histórica completa
g1 <- ggplot(infla, aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = COLORES) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "Evolución de la Inflación Interanual (1990–2025)",
    subtitle = "Colombia, México y Perú | Variación IPC respecto al mismo mes del año anterior",
    x = NULL, y = "Inflación interanual (%)", color = NULL,
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("grafico_linea.png", g1, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ grafico_linea.png\n")


# --- G2: Serie post-1995 (ajustada, sin hiperinflación peruana) 
g2 <- infla |>
  filter(anio >= 1995) |>
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = COLORES) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "Inflación Interanual Post-Estabilización (1995–2025)",
    x = NULL, y = "Inflación interanual (%)", color = NULL,
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("grafico_linea_ajustado.png", g2, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ grafico_linea_ajustado.png\n")


# --- G3: Los 90s en escala logarítmica
g3 <- infla |>
  filter(anio < 2000, inflacion > 0) |>
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = COLORES) +
  scale_y_log10(breaks = c(1, 5, 10, 25, 50, 100, 500),
                labels  = label_number(suffix = "%")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  annotation_logticks(sides = "l", color = "grey60", size = 0.3) +
  labs(
    title    = "Inflación Interanual: Los 90s",
    x = NULL, y = "Inflación interanual (%, escala log)", color = NULL,
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("linea_90s_log.png", g3, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ linea_90s_log.png\n")


# --- G4: Los 2000s 
g4 <- infla |>
  filter(anio >= 2000, anio < 2010) |>
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = COLORES) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Inflación Interanual: Los 2000s",
    subtitle = "Convergencia hacia la estabilidad de precios en los tres países",
    x = NULL, y = "Inflación interanual (%)", color = NULL,
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("linea_2000s.png", g4, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ linea_2000s.png\n")


# --- G5: Post-2010
g5 <- infla |>
  filter(anio >= 2010) |>
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = COLORES) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Inflación Interanual: Post-2010",
    subtitle = "Régimen de metas de inflación consolidado | Shocks de 2021–2022 por COVID y guerra",
    x = NULL, y = "Inflación interanual (%)", color = NULL,
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("linea_post2010.png", g5, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ linea_post2010.png\n")



# HISTOGRAMAS Y DISTRIBUCIONES


# --- G6: Histograma histórico completo (Zoom aplicado)

g6 <- ggplot(infla, aes(x = inflacion)) +
  # Cambiamos 'bins' por 'binwidth = 2' para que las barras tengan un ancho fijo
  geom_histogram(aes(y = after_stat(density)), binwidth = 2,
                 fill = "#4878CF", color = "white", alpha = 0.8) +
  geom_density(color = "#C44E52", linewidth = 0.8) +
  coord_cartesian(xlim = c(-10, 50)) +  # El zoom para ignorar los outliers visualmente
  facet_wrap(~pais, scales = "free") +
  labs(
    title    = "Distribución Histórica de la Inflación (1990–2025)",
    x = "Inflación interanual (%)", y = "Densidad",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("histograma_historico.png", g6, width = ANCHO + 2, height = ALTO, dpi = RES)
cat("✓ histograma_historico.png\n")


# --- G7: Histograma ajustado (desde 1996) 
g7 <- infla |>
  filter(anio >= 1996) |>
  ggplot(aes(x = inflacion)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35,
                 fill = "#4878CF", color = "white", alpha = 0.8) +
  geom_density(color = "#C44E52", linewidth = 0.8) +
  facet_wrap(~pais, scales = "free") +
  labs(
    title    = "Distribución de la Inflación (1996–2025)",
    x = "Inflación interanual (%)", y = "Densidad",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("histograma_ajustado.png", g7, width = ANCHO + 2, height = ALTO, dpi = RES)
cat("✓ histograma_ajustado.png\n")



# BOXPLOTS


# --- G8: Boxplot histórico por país 
g8 <- ggplot(infla, aes(x = pais, y = inflacion, fill = pais)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4, width = 0.5) +
  scale_fill_manual(values = COLORES) +
  labs(
    title    = "Boxplot de Inflación por País (1990–2025)",
    subtitle = "Los outliers superiores corresponden a episodios de alta inflación en los 90s",
    x = NULL, y = "Inflación interanual (%)",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("boxplot.png", g8, width = 8, height = ALTO, dpi = RES)
cat("✓ boxplot.png\n")


# --- G9: Boxplot ajustado (desde 1996)
g9 <- infla |>
  filter(anio >= 1996) |>
  ggplot(aes(x = pais, y = inflacion, fill = pais)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4, width = 0.5) +
  scale_fill_manual(values = COLORES) +
  labs(
    title    = "Boxplot de Inflación por País (1996–2025)",
    x = NULL, y = "Inflación interanual (%)",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("boxplot_ajustado.png", g9, width = 8, height = ALTO, dpi = RES)
cat("✓ boxplot_ajustado.png\n")



# DIAGRAMA DE FASE: INERCIA INFLACIONARIA EN PERÚ

fase_peru <- infla |>
  filter(pais == "Perú") |>
  mutate(
    infl_lag12 = lag(inflacion, 12),
    era        = if_else(anio < 2000, "Años 90 (Alta Inercia)", "Post-2000 (Anclaje de Precios)")
  ) |>
  filter(!is.na(infl_lag12), !is.na(inflacion))

g10 <- ggplot(fase_peru, aes(x = infl_lag12, y = inflacion, color = era)) +
  geom_point(alpha = 0.5, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("Años 90 (Alta Inercia)" = "#C44E52",
                                "Post-2000 (Anclaje de Precios)" = "#4878CF")) +
  labs(
    title    = "Diagrama de Fase: Inercia Inflacionaria en Perú",
    x = "Inflación interanual en t–12 (%)",
    y = "Inflación interanual en t (%)",
    color = NULL,
    caption = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("diagrama_fase_peru.png", g10, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ diagrama_fase_peru.png\n")


# ==============================================================================
# PARTE 1-C: INFLACIÓN vs M2
# ==============================================================================

# --- G11: Dispersión inflación vs var_m2 por país -----------------------------
g11 <- ggplot(merged, aes(x = var_m2, y = inflacion)) +
  geom_point(aes(color = pais), size = 0.9, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.7) +
  scale_color_manual(values = COLORES) +
  facet_wrap(~pais, scales = "free") +
  labs(
    title    = "Inflación vs Variación Mensual de M2",
    x = "Variación M2 mensual (%)", y = "Inflación interanual (%)",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("scatter_inflacion_m2.png", g11, width = ANCHO + 2, height = ALTO, dpi = RES)
cat("✓ scatter_inflacion_m2.png\n")


# --- G12: Dispersión con distinción temporal (quiebre estructural) -------------
g12 <- merged |>
  mutate(era = if_else(anio < 2000, "Años 90", "Post-2000")) |>
  ggplot(aes(x = var_m2, y = inflacion, color = era)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("Años 90" = "#C44E52", "Post-2000" = "#4878CF")) +
  facet_wrap(~pais, scales = "free") +
  labs(
    title    = "Quiebre Estructural: Relación M2–Inflación antes y después del año 2000",
    x = "Variación M2 mensual (%)", y = "Inflación interanual (%)",
    color = NULL,
    caption = "Fuente: CEPAL. Nota: cobertura de M2 varía por país (Colombia y Perú desde 1990; México desde 2000)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("scatter_m2_quiebre.png", g12, width = ANCHO + 2, height = ALTO, dpi = RES)
cat("✓ scatter_m2_quiebre.png\n")


# --- G13: Trayectoria Perú (connected scatter post-2000) ----------------------
tray_peru <- merged |>
  filter(pais == "Perú", anio >= 2000) |>
  mutate(era_reciente = anio >= 2015)

g13 <- ggplot() +
  geom_point(data = filter(tray_peru, !era_reciente),
             aes(x = var_m2, y = inflacion),
             color = "grey60", size = 0.8, alpha = 0.5) +
  geom_path(data = filter(tray_peru, era_reciente),
            aes(x = var_m2, y = inflacion),
            color = "#1f77b4", linewidth = 0.7, arrow = arrow(length = unit(0.2, "cm"),
                                                              ends = "last", type = "open")) +
  geom_point(data = filter(tray_peru, era_reciente),
             aes(x = var_m2, y = inflacion),
             color = "#1f77b4", size = 1.5) +
  labs(
    title    = "Trayectoria de la Relación M2–Inflación en Perú",
    subtitle = "Puntos grises: 2000–2014 | Trayectoria azul: 2015–2021 (con dirección temporal)",
    x = "Variación M2 mensual (%)", y = "Inflación interanual (%)",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("trayectoria_peru_m2.png", g13, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ trayectoria_peru_m2.png\n")


# --- G14: Estabilidad México con cotas ±2 SD ----------------------------------
mex_post2000 <- merged |> filter(pais == "México", anio >= 2000)
media_mex <- mean(mex_post2000$inflacion, na.rm = TRUE)
sd_mex    <- sd(mex_post2000$inflacion,   na.rm = TRUE)

g14 <- ggplot(mex_post2000, aes(x = fecha, y = inflacion)) +
  geom_ribbon(aes(ymin = media_mex - 2 * sd_mex,
                  ymax = media_mex + 2 * sd_mex),
              fill = "#C44E52", alpha = 0.12) +
  geom_hline(yintercept = media_mex,           linetype = "dashed", color = "black",   linewidth = 0.7) +
  geom_hline(yintercept = media_mex + 2*sd_mex, linetype = "dotted", color = "#C44E52", linewidth = 0.7) +
  geom_hline(yintercept = media_mex - 2*sd_mex, linetype = "dotted", color = "#C44E52", linewidth = 0.7) +
  geom_line(color = "#1f4e79", linewidth = 0.7) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  annotate("text", x = max(mex_post2000$fecha), y = media_mex + 0.3,
           label = paste0("Media = ", round(media_mex, 1), "%"),
           hjust = 1, size = 3.2, color = "black") +
  labs(
    title    = "Estabilidad y Shocks de Inflación en México (Post-2000)",
    subtitle = "Banda roja: ±2 desviaciones estándar históricasn| Puntos fuera = shocks inflacionarios",
    x = NULL, y = "Inflación interanual (%)",
    caption  = "Fuente: CEPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("cotas_mexico.png", g14, width = ANCHO, height = ALTO, dpi = RES)
cat("✓ cotas_mexico.png\n")


# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
cat("\n✅ Todos los gráficos exportados correctamente en:", getwd(), "\n")
cat("Archivos generados:\n")
cat("  Sección 1-B: grafico_linea.png, grafico_linea_ajustado.png,\n")
cat("               linea_90s_log.png, linea_2000s.png, linea_post2010.png,\n")
cat("               histograma_historico.png, histograma_ajustado.png,\n")
cat("               boxplot.png, boxplot_ajustado.png, diagrama_fase_peru.png\n")
cat("  Sección 1-C: scatter_inflacion_m2.png, scatter_m2_quiebre.png,\n")
cat("               trayectoria_peru_m2.png, cotas_mexico.png\n")


##EPH----------------------------------------------------------------

# ==============================================================================
# TP1 - MÉTODOS CUANTITATIVOS: GRÁFICOS EPH (R)
# ==============================================================================

# 1. Cargar librerías necesarias
library(tidyverse)
library(janitor)
library(scales)

# 2. Importar la base de datos

eph_raw <- read_delim("C:/TP1 - Métodos Cuantitativos/Datos/usu_individual_T325.txt", 
                      delim = ";", 
                      escape_double = FALSE, 
                      trim_ws = TRUE)

# 3. Limpieza y creación de variables (Replicando la lógica de Stata)
eph <- eph_raw |>
  clean_names() |> # Pasa todo a minúsculas
  # Filtramos solo ocupados con ingresos positivos
  filter(p21 > 0, !is.na(p21)) |> 
  mutate(
    # Etiquetas de región
    region_factor = factor(region,
                           levels = c(1, 40, 41, 42, 43, 44),
                           labels = c("Gran BsAs", "NOA", "NEA", "Cuyo", "Pampeana", "Patagonia")),
    
    # Creación de cat_empleo (Misma lógica exacta que tu Stata)
    cat_empleo = case_when(
      pp04c <= 6 & pp04a == 2 ~ 1,  # Privado Pequeño
      pp04c >= 7 & pp04a == 2 ~ 2,  # Privado Grande
      pp04a == 1              ~ 3,  # Sector Público
      TRUE                    ~ NA_real_
    ),
    
    cat_empleo_factor = factor(cat_empleo, 
                               levels = c(1, 2, 3), 
                               labels = c("Privado Pequeño", "Privado Grande", "Sector Público"))
  ) |>
  # Nos quedamos solo con los casos válidos para graficar
  filter(!is.na(cat_empleo_factor), !is.na(region_factor))


# ==============================================================================
# GRÁFICO 1: BARRAS DE INGRESO PROMEDIO
# ==============================================================================

# Primero calculamos el promedio ponderado agrupado por Región y Categoría
eph_promedios <- eph |>
  group_by(region_factor, cat_empleo_factor) |>
  summarise(ingreso_promedio = weighted.mean(p21, w = pondera, na.rm = TRUE), 
            .groups = "drop")

g_barras <- ggplot(eph_promedios, aes(x = region_factor, y = ingreso_promedio, fill = cat_empleo_factor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("Privado Pequeño" = "#4878CF", 
                               "Privado Grande"  = "#C44E52", 
                               "Sector Público"  = "#55A868")) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  labs(
    title    = "Ingreso Promedio por Tamaño de Establecimiento",
    x        = NULL,
    y        = "Ingreso Promedio ($)",
    fill     = "Sector y Tamaño",
    caption  = "Fuente: Elaboración propia en base a EPH-INDEC."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(vjust = 1)
  )

ggsave("barras_region_tamano.png", g_barras, width = 10, height = 6, dpi = 300)
cat("✓ barras_region_tamano.png guardado correctamente\n")


# ==============================================================================
# GRÁFICO 2: BOXPLOT CRUZADO (TRUNCADO EN 1.5M)
# ==============================================================================
library(quantreg)
g_boxplot <- eph |>
  filter(p21 < 1500000) |>
  ggplot(aes(x = cat_empleo_factor, y = p21, fill = cat_empleo_factor, weight = pondera)) +
  geom_boxplot(outlier.size = 0.7, outlier.alpha = 0.3, outlier.color = "gray50", width = 0.6) +
  facet_wrap(~region_factor) +
  scale_fill_manual(values = c("Privado Pequeño" = "#4878CF", 
                               "Privado Grande"  = "#C44E52", 
                               "Sector Público"  = "#55A868")) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","),
                     breaks = seq(0, 1500000, by = 500000)) +
  labs(
    title    = "Distribución de Ingresos por Región y Tamaño",
    x        = NULL,
    y        = "Ingreso Ocupación Principal ($)",
    caption  = "Fuente: Elaboración propia en base a EPH-INDEC."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    legend.position  = "none", # Apagamos leyenda porque el eje X ya lo explica
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1) # Inclinamos textos para que no se pisen
  )

ggsave("boxplot_region_tamano.png", g_boxplot, width = 10, height = 6, dpi = 300)
cat("✓ boxplot_region_tamano.png guardado correctamente\n")
