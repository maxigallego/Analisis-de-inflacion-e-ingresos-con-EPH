Informe elaborado para la Maestría en Economía Aplicada de la Universidad 
de Buenos Aires. El trabajo se divide en dos ejes:

1. **Análisis inflacionario** comparado entre Colombia, México y Perú
2. **Análisis de ingresos laborales** en Argentina para el tercer trimestre de 2025

## Fuentes de datos

### Inflación
- **CEPAL** — Base de datos de precios al consumidor para América Latina y el Caribe  
  https://statistics.cepal.org/portal/cepalstat

### Ingresos laborales — Argentina
- **EPH (Encuesta Permanente de Hogares) — INDEC**, 3° trimestre 2025  
  https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos  
  Base utilizada: `usu_individual_T325.txt` (base individual)

> Los microdatos no están incluidos en este repositorio. Deben descargarse 
> directamente desde los sitios oficiales indicados.

## Contenido del repositorio

| Archivo | Descripción |
|---|---|
| `InflaciónyEPH.do` | Código completo de análisis en Stata |
| `graficosR.R` | Código de visualizaciones en R |
| `barras_region_tamano.png` | Ingresos por región y tamaño de establecimiento |
| `boxplot_ajustado.png` | Distribución de ingresos ajustada |
| `scatter_inflacion_m2.png` | Dispersión inflación vs. M2 |
| `scatter_m2_quiebre.png` | M2 con quiebre estructural |
| `trayectoria_peru_m2.png` | Trayectoria de M2 en Perú |

## Requisitos

- **Stata** (v14 o superior) — análisis econométrico y EPH
- **R** — visualizaciones (ver `graficosR.R` para los paquetes utilizados)
