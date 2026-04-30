# ==============================================================================
# TP1 - MÉTODOS CUANTITATIVOS - VERSIÓN R
# Replica de gráficos del do-file de Stata
# ==============================================================================

# ---- 0. Setup ----------------------------------------------------------------
install.packages(c("tidyverse","readxl","scales","lubridate","ggthemes","haven"))
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

setwd("C:/TP1 - Métodos Cuantitativos/Datos")

# Carpeta para guardar gráficos
dir.create("graficos_R", showWarnings = FALSE)

# Tema y paleta consistentes para los 3 países
tema_base <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle    = element_text(color = "grey30", size = 11),
    plot.caption     = element_text(color = "grey40", size = 9, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )

paleta_paises <- c("Colombia" = "#1f77b4",
                   "México"   = "#2ca02c",
                   "Perú"     = "#d62728")

# Función helper para guardar
guardar <- function(plot, nombre, w = 10, h = 6) {
  ggsave(filename = file.path("graficos_R", nombre),
         plot = plot, width = w, height = h, dpi = 300, bg = "white")
}

# ==============================================================================
# 1. CARGA Y LIMPIEZA DE INFLACIÓN
# ==============================================================================
infla <- read_excel("Datos_Infla.xlsx") %>%
  mutate(
    anio  = as.numeric(anio),
    mes   = as.numeric(mes),
    fecha = make_date(anio, mes, 1),
    pais  = factor(pais, levels = c("Colombia", "México", "Perú"))
  ) %>%
  arrange(pais, fecha) %>%
  group_by(pais) %>%
  mutate(
    ipc_l12   = lag(ipc, 12),
    inflacion = ((ipc - ipc_l12) / ipc_l12) * 100
  ) %>%
  ungroup()

# Limpieza del outlier de México (cambio de base IPC)
cat("Valores sospechosos en México:\n")
infla %>% filter(pais == "México", inflacion < -5) %>%
  select(fecha, ipc, inflacion) %>% print()

infla <- infla %>%
  mutate(inflacion = if_else(pais == "México" & inflacion < -5,
                             NA_real_, inflacion))

# Variable período
infla <- infla %>%
  mutate(periodo = case_when(
    anio >= 1990 & anio <= 1999 ~ "1. Los 90s (Alta Inflación)",
    anio >= 2000 & anio <= 2009 ~ "2. Los 2000s (Estabilización)",
    anio >= 2010                ~ "3. Post-2010 (Metas de Inflación)"
  ))

# ==============================================================================
# 1.B) GRÁFICOS DE INFLACIÓN
# ==============================================================================

# --- Gráfico 1: Línea histórica completa ---
g1 <- infla %>%
  filter(!is.na(inflacion)) %>%
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = paleta_paises) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title    = "Evolución de la Inflación Mensual (1990-2025)",
       x = "Fecha", y = "Inflación Interanual (%)",
       caption  = "Fuente: Elaboración propia.") +
  tema_base
guardar(g1, "grafico_linea.png")

# --- Gráfico 2: Línea desde 1995 ---
g2 <- infla %>%
  filter(!is.na(inflacion), anio >= 1995) %>%
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = paleta_paises) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = "Inflación Mensual Post-Estabilización (1995-2025)",
       x = "Fecha", y = "Inflación Interanual (%)") +
  tema_base
guardar(g2, "grafico_linea_ajustado.png")

# --- Gráfico 3: Histograma histórico ---
g3 <- infla %>%
  filter(!is.na(inflacion)) %>%
  ggplot(aes(x = inflacion, fill = pais)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, color = "white", alpha = 0.7) +
  geom_density(color = "grey20", linewidth = 0.6, fill = NA) +
  facet_wrap(~ pais, scales = "free") +
  scale_fill_manual(values = paleta_paises, guide = "none") +
  labs(title = "Distribución Histórica de la Inflación (1990-2025)",
       x = "Inflación Interanual (%)", y = "Densidad") +
  tema_base
guardar(g3, "histograma_historico.png")

# --- Gráfico 4: Histograma desde 1996 ---
g4 <- infla %>%
  filter(!is.na(inflacion), anio >= 1996) %>%
  ggplot(aes(x = inflacion, fill = pais)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, color = "white", alpha = 0.7) +
  geom_density(color = "grey20", linewidth = 0.6, fill = NA) +
  facet_wrap(~ pais, scales = "free") +
  scale_fill_manual(values = paleta_paises, guide = "none") +
  labs(title = "Distribución de la Inflación (1996-2025)",
       x = "Inflación Interanual (%)", y = "Densidad") +
  tema_base
guardar(g4, "histograma_ajustado.png")

# --- Gráfico 5: Boxplot histórico ---
g5 <- infla %>%
  filter(!is.na(inflacion)) %>%
  ggplot(aes(x = pais, y = inflacion, fill = pais)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.8) +
  scale_fill_manual(values = paleta_paises, guide = "none") +
  labs(title = "Boxplot de Inflación por País",
       x = NULL, y = "Inflación (%)") +
  tema_base
guardar(g5, "boxplot.png", w = 8, h = 6)

# --- Gráfico 6: Boxplot desde 1996 ---
g6 <- infla %>%
  filter(!is.na(inflacion), anio >= 1996) %>%
  ggplot(aes(x = pais, y = inflacion, fill = pais)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.8) +
  scale_fill_manual(values = paleta_paises, guide = "none") +
  labs(title = "Boxplot de Inflación (1996-2025)",
       x = NULL, y = "Inflación Interanual (%)") +
  tema_base
guardar(g6, "boxplot_ajustado.png", w = 8, h = 6)

# --- Gráfico 7: Diagrama de fase Perú (inercia inflacionaria) ---
peru_fase <- infla %>%
  filter(pais == "Perú", !is.na(inflacion)) %>%
  arrange(fecha) %>%
  mutate(infla_l12 = lag(inflacion, 12),
         epoca = if_else(anio < 2000, "Años 90 (Alta Inercia)",
                         "Post 2000 (Anclaje de Precios)"))

g7 <- peru_fase %>%
  filter(!is.na(infla_l12)) %>%
  ggplot(aes(x = infla_l12, y = inflacion, color = epoca)) +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Años 90 (Alta Inercia)" = "#d62728",
                                "Post 2000 (Anclaje de Precios)" = "#1f77b4")) +
  labs(title = "Diagrama de Fase: Inercia Inflacionaria en Perú",
       x = "Inflación Interanual en t-1 (%)",
       y = "Inflación Interanual en t (%)") +
  tema_base
guardar(g7, "diagrama_fase_peru.png")

# --- Gráfico 8: Línea 90s en escala log ---
g8 <- infla %>%
  filter(!is.na(inflacion), anio < 2000, inflacion > 0) %>%
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = paleta_paises) +
  scale_y_log10(breaks = c(1, 5, 10, 25, 50, 100, 500, 5000),
                labels = comma_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Inflación Interanual: Los 90s (Escala Log)",
       x = "Año", y = "Inflación (%) - Escala Log") +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(g8, "linea_90s_log.png")

# --- Gráfico 9: Línea 2000s ---
g9 <- infla %>%
  filter(!is.na(inflacion), anio >= 2000, anio < 2010) %>%
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = paleta_paises) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Inflación Interanual: Los 2000s",
       x = "Año", y = "Inflación Interanual (%)") +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(g9, "linea_2000s_limpia.png")

# --- Gráfico 10: Línea Post-2010 ---
g10 <- infla %>%
  filter(!is.na(inflacion), anio >= 2010) %>%
  ggplot(aes(x = fecha, y = inflacion, color = pais)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = paleta_paises) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Inflación Interanual: Post-2010",
       x = "Año", y = "Inflación Interanual (%)") +
  tema_base
guardar(g10, "linea_post2010_limpia.png")

# ==============================================================================
# 1.C) RELACIÓN CON M2
# ==============================================================================

m2 <- read_excel("Datos_M2.xlsx") %>%
  mutate(
    anio  = as.numeric(anio),
    mes   = as.numeric(mes),
    fecha = make_date(anio, mes, 1),
    pais  = factor(pais, levels = c("Colombia", "México", "Perú"))
  ) %>%
  arrange(pais, fecha) %>%
  group_by(pais) %>%
  mutate(var_m2 = ((m2 - lag(m2)) / lag(m2)) * 100) %>%
  ungroup()

# Merge inflación + M2 (equivalente al merge 1:1 por id_pais y fecha)
datos <- infla %>%
  inner_join(m2 %>% select(pais, fecha, m2, var_m2),
             by = c("pais", "fecha"))

# --- Gráfico 11: Scatter inflación vs M2 (todos los países) ---
g11 <- datos %>%
  filter(!is.na(inflacion), !is.na(var_m2)) %>%
  ggplot(aes(x = var_m2, y = inflacion)) +
  geom_point(aes(color = pais), alpha = 0.5, size = 1.4) +
  geom_smooth(method = "lm", color = "black",
              se = TRUE, linewidth = 0.7) +
  facet_wrap(~ pais, scales = "free") +
  scale_color_manual(values = paleta_paises, guide = "none") +
  labs(title = "Inflación vs Variación de M2",
       x = "Variación M2 Mensual (%)",
       y = "Inflación Interanual (%)") +
  tema_base
guardar(g11, "scatter_inflacion_m2.png", w = 12, h = 5)

# --- Gráfico 12: Quiebre estructural M2-Inflación (antes/después 2000) ---
datos_quiebre <- datos %>%
  filter(!is.na(inflacion), !is.na(var_m2)) %>%
  mutate(epoca = if_else(anio < 2000, "Años 90", "Post-2000"))

g12 <- ggplot(datos_quiebre,
              aes(x = var_m2, y = inflacion, color = epoca)) +
  geom_point(alpha = 0.45, size = 1.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ pais, scales = "free") +
  scale_color_manual(values = c("Años 90" = "#d62728",
                                "Post-2000" = "#1f77b4")) +
  labs(title    = "Quiebre Estructural: Relación M2-Inflación antes y después del año 2000",
       x = "Variación M2 mensual (%)",
       y = "Inflación interanual (%)",
       caption  = "Fuente: CEPAL. Nota: cobertura de M2 varía por país (Colombia y Perú desde 1990; México desde 2000).") +
  tema_base
guardar(g12, "scatter_m2_quiebre.png", w = 12, h = 5)

# --- Gráfico 13: Trayectoria M2-Inflación Perú ---
peru_m2_hist   <- datos %>% filter(pais == "Perú", anio >= 2000,
                                   !is.na(inflacion), !is.na(var_m2))
peru_m2_recien <- datos %>% filter(pais == "Perú", anio >= 2015,
                                   !is.na(inflacion), !is.na(var_m2)) %>%
  arrange(fecha)

g13 <- ggplot() +
  geom_point(data = peru_m2_hist,
             aes(x = var_m2, y = inflacion,
                 color = "Histórico (2000-2025)"),
             alpha = 0.3, size = 1) +
  geom_path(data = peru_m2_recien,
            aes(x = var_m2, y = inflacion,
                color = "Trayectoria reciente (2015-2025)"),
            linewidth = 0.6) +
  geom_point(data = peru_m2_recien,
             aes(x = var_m2, y = inflacion,
                 color = "Trayectoria reciente (2015-2025)"),
             size = 1.4) +
  scale_color_manual(values = c("Histórico (2000-2025)" = "grey60",
                                "Trayectoria reciente (2015-2025)" = "#1f77b4")) +
  labs(title = "Trayectoria de la Relación M2-Inflación en Perú",
       x = "Variación M2 (%)", y = "Inflación (%)") +
  tema_base
guardar(g13, "trayectoria_peru_m2.png")

# --- Gráfico 14: Cotas de estabilidad México ---
mex_post2000 <- datos %>%
  filter(pais == "México", anio >= 2000, !is.na(inflacion))

mex_stats <- mex_post2000 %>%
  summarise(media = mean(inflacion, na.rm = TRUE),
            sd    = sd(inflacion,   na.rm = TRUE))

mex_post2000 <- mex_post2000 %>%
  mutate(media   = mex_stats$media,
         cota_sup = mex_stats$media + 2 * mex_stats$sd,
         cota_inf = mex_stats$media - 2 * mex_stats$sd)

g14 <- ggplot(mex_post2000, aes(x = fecha)) +
  geom_line(aes(y = inflacion, color = "Inflación"), linewidth = 0.6) +
  geom_line(aes(y = media,    color = "Media Histórica"),
            linetype = "dashed", linewidth = 0.6) +
  geom_line(aes(y = cota_sup, color = "Límites (±2 SD)"),
            linetype = "dotted", linewidth = 0.7) +
  geom_line(aes(y = cota_inf, color = "Límites (±2 SD)"),
            linetype = "dotted", linewidth = 0.7) +
  scale_color_manual(values = c("Inflación"        = "#1f3a5f",
                                "Media Histórica"  = "black",
                                "Límites (±2 SD)"  = "#d62728")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = "Estabilidad y Shocks de Inflación en México (Post-2000)",
       x = "Año", y = "Inflación Interanual (%)") +
  tema_base
guardar(g14, "cotas_mexico.png")

# ==============================================================================
# 2. EPH - ENCUESTA PERMANENTE DE HOGARES (T3 2025)
# ==============================================================================

eph <- read_delim("usu_individual_T325.txt", delim = ";",
                  show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  filter(p21 > 0, !is.na(p21))

# Etiquetas de región
eph <- eph %>%
  mutate(region = factor(region,
                         levels = c(1, 40, 41, 42, 43, 44),
                         labels = c("Gran BsAs", "NOA", "NEA",
                                    "Cuyo", "Pampeana", "Patagonia")))

# Variable categoría empleo (réplica exacta del do)
eph <- eph %>%
  mutate(cat_empleo = case_when(
    pp04c <= 6 & pp04a == 2 ~ "Privado Pequeño",
    pp04c >= 7 & pp04a == 2 ~ "Privado Grande",
    pp04a == 1              ~ "Sector Público"
  )) %>%
  mutate(cat_empleo = factor(cat_empleo,
                             levels = c("Privado Pequeño",
                                        "Privado Grande",
                                        "Sector Público")))

paleta_empleo <- c("Privado Pequeño" = "#9ecae1",
                   "Privado Grande"  = "#3182bd",
                   "Sector Público"  = "#e6550d")

# --- Gráfico 15: Barras ingreso promedio por región y tamaño ---
eph_barras <- eph %>%
  filter(!is.na(cat_empleo), !is.na(region)) %>%
  group_by(region, cat_empleo) %>%
  summarise(ingreso_prom = weighted.mean(p21, pondera, na.rm = TRUE),
            .groups = "drop")

g15 <- ggplot(eph_barras,
              aes(x = region, y = ingreso_prom, fill = cat_empleo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_y_continuous(labels = label_number(big.mark = ".",
                                           decimal.mark = ",",
                                           prefix = "$")) +
  scale_fill_manual(values = paleta_empleo) +
  labs(title    = "Ingreso Promedio por Tamaño de Establecimiento",
       subtitle = "Apertura al interior de cada Región – T3 2025",
       x = NULL, y = "Ingreso Promedio ($)",
       caption  = "Fuente: Elaboración propia en base a EPH-INDEC.") +
  tema_base
guardar(g15, "barras_region_tamano.png", w = 11, h = 6)

# --- Gráfico 16: Boxplot ingresos por región y tamaño ---
g16 <- eph %>%
  filter(!is.na(cat_empleo), !is.na(region), p21 < 1500000) %>%
  ggplot(aes(x = cat_empleo, y = p21, fill = cat_empleo,
             weight = pondera)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.4,
               outlier.color = "grey60") +
  facet_wrap(~ region, nrow = 2) +
  scale_y_continuous(labels = label_number(big.mark = ".",
                                           decimal.mark = ",",
                                           prefix = "$"),
                     breaks = seq(0, 1500000, 500000)) +
  scale_fill_manual(values = paleta_empleo, guide = "none") +
  labs(title    = "Distribución de Ingresos por Región y Tamaño",
       x = NULL, y = "Ingreso Ocupación Principal ($)",
       caption  = "Fuente: EPH-INDEC. Eje Y truncado en $1.500.000 para visualización.") +
  tema_base +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))
guardar(g16, "boxplot_region_tamano.png", w = 12, h = 7)

cat("\n✅ Listo. Gráficos guardados en ./graficos_R/\n")
