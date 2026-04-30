* ==============================================================================
*Análisis de procesos y metas de inflación
* ==============================================================================

clear all
set more off

cd "C:\TP1 - Métodos Cuantitativos\Datos"

* ==============================================================================
* 1. Países seleccionados: Colombia, Perú, México
* ==============================================================================

import excel "Datos_Infla.xlsx", firstrow clear
save "Datos_Infla.dta", replace
clear all
use "Datos_Infla.dta"

destring anio mes, replace
gen fecha = ym(anio, mes)
format fecha %tm

encode pais, gen(id_pais)
xtset id_pais fecha
label list

gen inflacion = ((ipc - L12.ipc) / L12.ipc) * 100

gen periodo = "1. Los 90s (Alta Inflación" if anio >= 1990 & anio <= 1999
replace periodo = "2. Los 2000s (Estabilización)" if anio >= 2000 & anio <= 2009
replace periodo = "3. Post-2010 (Metas de Inflación)" if anio >= 2010

save "inflacion_limpia.dta", replace
export excel using "bases_limpias_TP1.xlsx", ///
    sheet("Inflacion") sheetreplace firstrow(variables)

* ==============================================================================
* 1 A) ESTADÍSTICA DESCRIPTIVA
* ==============================================================================
* Medidas de tendencia central, dispersión y superiores.
tabstat inflacion, by(pais) stat(mean p50 sd var min max skewness kurtosis) format(%9.2f)

bysort pais: tabstat inflacion, by(periodo) stat(mean sd cv min max) format(%9.2f)

display "=================================================="
display "ANÁLISIS DE DÉCADAS: COLOMBIA"
display "=================================================="
tabstat inflacion if pais == "Colombia", by(periodo) stat(mean sd cv min max) format(%9.2f)

display "=================================================="
display "ANÁLISIS DE DÉCADAS: MÉXICO"
display "=================================================="
tabstat inflacion if pais == "México", by(periodo) stat(mean sd cv min max) format(%9.2f)

display "=================================================="
display "ANÁLISIS DE DÉCADAS: PERÚ"
display "=================================================="
tabstat inflacion if pais == "Perú", by(periodo) stat(mean sd cv min max) format(%9.2f)

* ==============================================================================
* 1 B) PRESENTACIÓN GRÁFICA
* ==============================================================================

twoway (line inflacion fecha if pais == "Colombia") ///
       (line inflacion fecha if pais == "Perú") ///
       (line inflacion fecha if pais == "México"), ///
       title("Evolución de la Inflación Mensual (1990-2025)") ///
       ytitle("Inflación Mensual (%)") xtitle("Fecha") ///
       xlabel(#10, format(%tm) angle(45)) ///
       legend(label(1 "Colombia") label(2 "Perú") label(3 "México"))

graph export "grafico_linea.png", replace

twoway (line inflacion fecha if pais == "Colombia" & anio >= 1995) ///
       (line inflacion fecha if pais == "Perú" & anio >= 1995) ///
       (line inflacion fecha if pais == "México" & anio >= 1995), ///
       title("Inflación Mensual Post-Estabilización (1995-2025)") ///
       ytitle("Inflación Mensual (%)") xtitle("Fecha") ///
       xlabel(#10, format(%tm) angle(45)) ///
       legend(label(1 "Colombia") label(2 "Perú") label(3 "México"))
graph export "grafico_linea_ajustado.png", replace


histogram inflacion, by(pais, title("Distribución Histórica de la Inflación (1990-2025)")) ///
    kdensity xtitle("Inflación Interanual (%)")
graph export "histograma_historico.png", replace

histogram inflacion if anio >= 1996, by(pais, title("Distribución de la Inflación (1996-2025)")) ///
    kdensity xtitle("Inflación Interanual (%)")
graph export "histograma_ajustado.png", replace

graph box inflacion, over(pais) title("Boxplot de Inflación por País") ///
    ytitle("Inflación (%)")
graph export "boxplot.png", replace

graph box inflacion if anio >= 1996, over(pais) title("Boxplot de Inflación (1996-2025)") ///
    ytitle("Inflación Interanual (%)")
graph export "boxplot_ajustado.png", replace

sort id_pais fecha
twoway (scatter inflacion L12.inflacion if pais == "Perú" & anio < 2000, mcolor(red%50)) ///
       (scatter inflacion L12.inflacion if pais == "Perú" & anio >= 2000, mcolor(blue%50)), ///
       title("Diagrama de Fase: Inercia Inflacionaria en Perú") ///
       ytitle("Inflación Interanual en t (%)") xtitle("Inflación Interanual en t-1 (%)") ///
       legend(label(1 "Años 90 (Alta Inercia)") label(2 "Post 2000 (Anclaje de Precios)"))
graph export "diagrama_fase_peru.png", replace

twoway (line inflacion fecha if pais == "Colombia" & anio < 2000) ///
       (line inflacion fecha if pais == "México" & anio < 2000) ///
       (line inflacion fecha if pais == "Perú" & anio < 2000), ///
       title("Inflación Interanual: Los 90s (Escala Log)") ///
       ytitle("Inflación (%) - Escala Log") xtitle("Año") ///
       ylabel(1 5 10 25 50 100 500) yscale(log) /// 
       xlabel(#10, format(%tmY)) ///
       legend(label(1 "Colombia") label(2 "México") label(3 "Perú"))
graph export "linea_90s_log.png", replace

twoway (line inflacion fecha if pais == "Colombia" & anio >= 2000 & anio < 2010) ///
       (line inflacion fecha if pais == "México" & anio >= 2000 & anio < 2010) ///
       (line inflacion fecha if pais == "Perú" & anio >= 2000 & anio < 2010), ///
       title("Inflación Interanual: Los 2000s") ///
       ytitle("Inflación Interanual (%)") xtitle("Año") ///
       xlabel(#10, format(%tmY)) ///
       legend(label(1 "Colombia") label(2 "México") label(3 "Perú"))
graph export "linea_2000s_limpia.png", replace

twoway (line inflacion fecha if pais == "Colombia" & anio >= 2010) ///
       (line inflacion fecha if pais == "México" & anio >= 2010) ///
       (line inflacion fecha if pais == "Perú" & anio >= 2010), ///
       title("Inflación Interanual: Post-2010") ///
       ytitle("Inflación Interanual (%)") xtitle("Año") ///
       xlabel(#10, format(%tmY)) ///
       legend(label(1 "Colombia") label(2 "México") label(3 "Perú"))
graph export "linea_post2010_limpia.png", replace

* ==============================================================================
* 1 C) RELACIÓN CON VARIABLE MACROECONÓMICA (M2)
* ==============================================================================
clear
import excel "Datos_M2.xlsx", firstrow clear

destring anio mes, replace force
gen fecha = ym(anio, mes)
format fecha %tm
encode pais, gen(id_pais)
xtset id_pais fecha

gen var_m2 = ((m2 - L.m2) / L.m2) * 100
save "m2_limpia.dta", replace
export excel using "bases_limpias_TP1.xlsx", ///
    sheet("M2") sheetreplace firstrow(variables)

use "inflacion_limpia.dta", clear
merge 1:1 id_pais fecha using "m2_limpia.dta"

keep if _merge == 3 
tab _merge
drop _merge

* Gráfico de dispersión
twoway (scatter inflacion var_m2) (lfit inflacion var_m2), ///
    by(pais, title("Inflación vs Variación de M2")) ///
    ytitle("Inflación Mensual (%)") xtitle("Variación M2 Mensual (%)")
graph export "scatter_inflacion_m2.png", replace

pwcorr inflacion var_m2, sig

levelsof id_pais, local(paises)
foreach p of local paises {
    display "========================================"
    display "Correlación para el país ID: `p'"
    pwcorr inflacion var_m2 if id_pais == `p', sig
}

display "==== ANÁLISIS DE QUIEBRE ESTRUCTURAL (AÑOS 90 VS POST 2000) ===="
levelsof id_pais, local(paises)
capture pwcorr inflacion var_m2 if id_pais == `p' & anio < 2000, sig
    if _rc != 0 display "Aviso: No hay observaciones suficientes para este período."
foreach p of local paises {
    display "----------------------------------------"
    display "PAÍS ID `p' - CORRELACIÓN SOLO AÑOS 90"
   capture pwcorr inflacion var_m2 if id_pais == `p' & anio < 2000, sig
	 
    display "PAÍS ID `p' - CORRELACIÓN POST 2000"
   capture pwcorr inflacion var_m2 if id_pais == `p' & anio >= 2000, sig
}

*Trayectoria de Perú: De una hiperinflación en los 90 a las metas del Banco Central
sort id_pais fecha
twoway (scatter inflacion var_m2 if pais == "Perú" & anio >= 2000, msize(tiny) mcolor(gray%30)) ///
       (connected inflacion var_m2 if pais == "Perú" & anio >= 2015, lcolor(blue) msize(small)), ///
       title("Trayectoria de la Relación M2-Inflación en Perú") ///
       ytitle("Inflación (%)") xtitle("Variación M2 (%)") ///
       legend(label(1 "Histórico (2000-2025)") label(2 "Trayectoria reciente (2015-2025)"))
graph export "trayectoria_peru_m2.png", replace
	   
* --- MÉXICO: ANÁLISIS DE ESTABILIDAD ---
summarize inflacion if pais == "México" & anio >= 2000
gen media_mex = r(mean)
gen cota_sup = r(mean) + 2*r(sd)
gen cota_inf = r(mean) - 2*r(sd)

twoway (line inflacion fecha if pais == "México" & anio >= 2000, lcolor(navy)) ///
       (line media_mex fecha if pais == "México" & anio >= 2000, lpattern(dash) lcolor(black)) ///
       (line cota_sup fecha if pais == "México" & anio >= 2000, lpattern(dot) lcolor(red)) ///
       (line cota_inf fecha if pais == "México" & anio >= 2000, lpattern(dot) lcolor(red)), ///
       title("Estabilidad y Shocks de Inflación en México (Post-2000)") ///
       ytitle("Inflación Interanual (%)") xtitle("Año") ///
       xlabel(#10, format(%tmY)) ///
       legend(label(1 "Inflación") label(2 "Media Histórica") label(3 "Límites (+/- 2 SD)"))

graph export "cotas_mexico.png", replace


********************************************************************************
* 2) ENCUESTA PERMANENTE DE HOGARES (EPH)
********************************************************************************
clear all
import delimited "usu_individual_T325.txt", clear 

rename *, lower

* Filtramos: Solo ocupados con ingresos positivos en la ocupación principal
keep if p21 > 0 & p21 != .
sum pondera

label define reg_lab 1 "Gran BsAs" 40 "NOA" 41 "NEA" 42 "Cuyo" 43 "Pampeana" 44 "Patagonia"
label values region reg_lab

* Creación de Variable de Agrupación: Tamaño y Sector
* PP04A: 1=Estatal, 2=Privado | PP04C: Tamaño
cap drop cat_empleo
gen cat_empleo = .
replace cat_empleo = 1 if pp04c <= 6 & pp04a == 2         
replace cat_empleo = 2 if pp04c >= 7 & pp04a == 2         
replace cat_empleo = 3 if pp04a == 1                      
label define cat_lab 1 "Privado Pequeño" 2 "Privado Grande" 3 "Sector Público"
label values cat_empleo cat_lab
save "eph_limpia.dta", replace
export excel using "bases_limpias_TP1.xlsx", ///
    sheet("EPH") sheetreplace firstrow(variables)


graph bar (mean) p21 [aw=pondera], ///
    over(cat_empleo, label(labsize(vsmall))) ///
    over(region, label(labsize(small))) ///
    asyvars /// 
    title("Ingreso Promedio por Tamaño de Establecimiento", size(medium)) ///
    subtitle("Apertura al interior de cada Región - T3 2025", size(small)) ///
    ytitle("Ingreso Promedio ($)") ///
    ylabel(, format(%12.0fc) labsize(small)) ///
    legend(title("Sector y Tamaño", size(vsmall)) size(small) rows(1)) ///
    note("Fuente: Elaboración propia en base a EPH-INDEC.")
	graph export "barras_region_tamano.png", replace
	
	graph box p21 [aw=pondera] if p21 < 1500000, ///
    over(cat_empleo, label(labsize(vsmall) angle(45))) ///
    by(region, title("Distribución de Ingresos por Región y Tamaño", size(medium)) ///
    note("Fuente: EPH-INDEC. Eje X truncado en $1.500.000 para visualización.", size(vsmall))) ///
    ytitle("Ingreso Ocupación Principal ($)", size(small)) ///
    ylabel(0(500000)1500000, format(%12.0fc) labsize(vsmall)) ///
    marker(1, msize(tiny) mcolor(gs12)) ///
    legend(off)
	graph export "boxplot_region_tamano.png", replace


* 2.B) Intervalos de Confianza (90%, 95% y 99%)
display "=== INTERVALOS DE CONFIANZA ==="
ci means p21 [aw=pondera], level(90)
ci means p21 [aw=pondera], level(95)
ci means p21 [aw=pondera], level(99)

display "=== INTERVALOS DE CONFIANZA POR REGIÓN ==="
by region, sort: ci means p21 [aw=pondera], level(90)
by region, sort: ci means p21 [aw=pondera], level(95)
by region, sort: ci means p21 [aw=pondera], level(99)

* O utilizamos esta forma alternativa, la cual en mi opinión presenta una visualización más amigable.
display "=== INTERVALOS DE CONFIANZA POR REGIÓN ==="
mean p21 [pw=pondera], over(region) level(90)
mean p21 [pw=pondera], over(region) level(95)
mean p21 [pw=pondera], over(region) level(99)

********************************************************************************
* INTERVALOS DE CONFIANZA CRUZADOS (Región y Tamaño)
********************************************************************************
mean p21 [pw=pondera], over(region cat_empleo) level(90)
mean p21 [pw=pondera], over(region cat_empleo) level(95)
mean p21 [pw=pondera], over(region cat_empleo) level(99)


* 2.C) Test de Diferencia de Medias (p-valor) con ponderadores
* Asumimos como hipótesis nula el valor del salario mínimo vital y movil, es decir $357.800
svyset [pweight=pondera]
svy: mean p21
lincom [p21] - 357800

* --- TAMAÑO DEL ESTABLECIMIENTO ---
svy: mean p21, over(cat_empleo)

display "--- Privado Pequeño (1) vs Público (2) ---"
lincom [c.p21@1.cat_empleo] - [c.p21@2.cat_empleo]

display "--- Privado Pequeño (1) vs Privado Grande (3) ---"
lincom [c.p21@1.cat_empleo] - [c.p21@3.cat_empleo]

display "--- Público (2) vs Privado Grande (3) ---"
lincom [c.p21@2.cat_empleo] - [c.p21@3.cat_empleo]

* --- REGIONES ---
svy: mean p21, over(region)

display "--- Patagonia (44) vs NEA (41) ---"
lincom [c.p21@44.region] - [c.p21@41.region]

display "--- Patagonia (44) vs NOA (40) ---"
lincom [c.p21@44.region] - [c.p21@40.region]

display "--- GBA (1) vs Pampeana (43) ---"
lincom [c.p21@1.region] - [c.p21@43.region]

display "--- GBA (1) vs Patagonia (44) ---"
lincom [c.p21@1.region] - [c.p21@44.region]

* ==============================================================================
* TEST DE DIFERENCIA DE MEDIAS SIN PONDERADORES USANDO TTEST
* ==============================================================================
ttest p21 == 357800

*******************************************************************************************************************
*Vemos que en este caso varía considerablemente la media y el desvío estándar respecto de utilizar ponderadores. No obstante, el p-valor en ambos casos es 0, lo que refleja que el salario mínimo en Argentina está obsoleto.
*******************************************************************************************************************

* --- TAMAÑO DEL ESTABLECIMIENTO ---
display "=== SIN PONDERADORES: PRIVADO PEQUEÑO vs PRIVADO GRANDE ==="
ttest p21 if cat_empleo == 1 | cat_empleo == 2, by(cat_empleo)

display "=== SIN PONDERADORES: PRIVADO PEQUEÑO vs SECTOR PÚBLICO ==="
ttest p21 if cat_empleo == 1 | cat_empleo == 3, by(cat_empleo)

display "=== SIN PONDERADORES: PRIVADO GRANDE vs SECTOR PÚBLICO ==="
ttest p21 if cat_empleo == 2 | cat_empleo == 3, by(cat_empleo)

* --- REGIONES ---
display "=== SIN PONDERADORES: PATAGONIA vs NEA ==="
ttest p21 if region == 44 | region == 41, by(region)

display "=== SIN PONDERADORES: PATAGONIA vs NOA ==="
ttest p21 if region == 44 | region == 40, by(region)

display "=== SIN PONDERADORES: GBA vs PAMPEANA ==="
ttest p21 if region == 1 | region == 43, by(region)

display "=== SIN PONDERADORES: GBA vs PATAGONIA ==="
ttest p21 if region == 1 | region == 44, by(region)