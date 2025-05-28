### Title: Proyecto Politica Comparada III
### Author: Alvaro Perez
### Date: May 20, 2025

# ******************************************************************************
# ---- I. Libraries ----
# ******************************************************************************

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
install.packages("patchwork")
library(patchwork)
library(stringr)
install.packages("fixest")
library(fixest)
library(broom)
install.packages("gsynth")
library(gsynth)
library(lmtest)
library(sandwich)
library(purrr)
install.packages("did")
library(did)
install.packages("did2s")
library(did2s)
install.packages("DIDmultiplegtDYN")
library(DIDmultiplegtDYN)
install.packages("ecic")
library(ecic)

# ******************************************************************************
# ---- I. Loading data ----
# ******************************************************************************

incumb <- read_xlsx("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/aymu1989-on.incumbents.xlsx")

efipem15 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2015.csv")

efipem16 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2016.csv")

efipem17 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2017.csv")

efipem18 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2018.csv")

efipem19 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2019.csv")

efipem20 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2020.csv")

efipem21 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2021.csv")

efipem22 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2022.csv")

efipem23 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_2023.csv")

muni <- read_xlsx("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/11 - ONCEAVO SEMESTRE/Política Comparada III/Proyecto/Catalogo_MUN.xlsx")

# ******************************************************************************
# ---- II. Cleaning data ----
# ******************************************************************************

### Municipalities ###

# Base de datos con municipios e identificador 
muni <- muni %>% 
  select(,-POB_TOTAL) %>% 
  rename("edomun_id"="CVEGEO")

years <- seq(2015,2023)

# Repetición de observación de 2015 a 2023
muni_years <- expand_grid(muni, year = years)


# ******************************************************************************
### Join with Magar's data base ###

incumb <- incumb %>% filter(yr >= 2015 & yr <= 2023)

incumb <- incumb %>%
  mutate(
    edon = str_pad(as.character(edon), width = 2, pad = "0"),
    edomun_id = str_c(edon,mun...26)
  )

# Merge con la base de municipios
reelect_db <- muni_years %>% 
  left_join(incumb %>% 
              select(reelection, edomun_id, yr),
            by = c("edomun_id","year"="yr"))

# Se repite la observación anterior en "reelection" hasta hallar una nueva
reelect_panel <- reelect_db %>% 
  group_by(edomun_id) %>% 
  arrange(edomun_id,year) %>% 
  fill(reelection, .direction = "down") %>% 
  ungroup()

# Imputo los NAs
reelect_panel <- reelect_panel %>% 
  mutate(
    reelection = replace_na(reelection,0)
  )

# ******************************************************************************
### EFIPEM ###
expenditure <- c("Total de egresos",
                 "Estímulos",
                 "Dependencias diversas",
                 "Obra pública en bienes de dominio público",
                 "Transferencias, asignaciones, subsidios y otras ayudas")

revenue <- c("Impuestos",
             "Derechos por el uso, goce, aprovechamiento o explotación de bienes de dominio público",
             "Licencias y permisos",
             "Multas",
             "Impuesto predial",
             "Participaciones federales",
             "Participaciones estatales",
             "Aportaciones federales y estatales") 

pubfin <- c(expenditure,revenue)

# Construir base finance con todos los indicadores 
filtrar_pubfin <- function(df) {
  df %>%
    filter(DESCRIPCION_CATEGORIA %in% pubfin)
}

years <- 15:23

dfs_list <- mget(paste0("efipem",years))

filtered_list <- lapply(dfs_list, filtrar_pubfin)

# Unir todo en un data frame
finance <- bind_rows(filtered_list, .id = "origen")

finance <- finance %>% 
  mutate(log_valor = log(VALOR),
         edomun_id = str_c(ID_ENTIDAD,ID_MUNICIPIO))

## "finance": contiene el los 8 rubros de finanzas públicas por municipio de 2015 a 2018

# Filtraciones por variable  
#1
egresos <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Total de egresos") %>% 
  rename("total_egresos" = "VALOR",
         "log_egresos" = "log_valor")
#2
estimulos <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Estímulos") %>% 
  rename("estimulos" = "VALOR",
         "log_estimulos" = "log_valor")
#3
nomina <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Dependencias diversas") %>% 
  rename("nomina" = "VALOR",
         "log_nomina" = "log_valor")
#4
subsidios <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Transferencias, asignaciones, subsidios y otras ayudas") %>% 
  rename("subsidios" = "VALOR",
         "log_subsidios" = "log_valor")
#5
obra <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público") %>% 
  rename("obra" = "VALOR",
         "log_obra" = "log_valor")
#6
impuestos <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Impuestos") %>% 
  rename("impuestos" = "VALOR",
         "log_impuestos" = "log_valor")
#7
predial <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Impuesto predial") %>% 
  rename("predial" = "VALOR",
         "log_predial" = "log_valor")
#8
usufructo <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Derechos por el uso, goce, aprovechamiento o explotación de bienes de dominio público") %>% 
  rename("usufructo" = "VALOR",
         "log_usufructo" = "log_valor")
#9
permisos <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Licencias y permisos") %>% 
  rename("permisos" = "VALOR",
         "log_permisos" = "log_valor")
#10
multas <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Multas") %>% 
  rename("multas" = "VALOR",
         "log_multas" = "log_valor")
#11
parti_fed <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Participaciones federales") %>% 
  rename("parti_fed" = "VALOR",
         "log_parti_fed" = "log_valor")
#12
parti_est <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Participaciones estatales") %>% 
  rename("parti_est" = "VALOR",
         "log_parti_est" = "log_valor")
#13
aporta <- finance %>% 
  select(edomun_id,ANIO,DESCRIPCION_CATEGORIA,VALOR,log_valor) %>% 
  filter(DESCRIPCION_CATEGORIA == "Aportaciones federales y estatales") %>% 
  rename("aporta" = "VALOR",
         "log_aporta" = "log_valor")

### Heteroscedasticidad ###

## Prueba Breusch-Pagan
mod_bp <- feols(total_egresos ~ reelection | edomun_id + year, data = incumb_fin)
res <- residuals(mod_bp)
fitted_vals <- fitted(mod_bp)
bptest(res ~ fitted_vals)

# H0: homosedasticidad, si p-value < 0.05, hay heteroscedasticidad

# ******************************************************************************
# ---- III. Descriptive statistics for 'finance' ----
# ******************************************************************************

finance_means <- finance %>% 
  group_by(DESCRIPCION_CATEGORIA, ANIO) %>% 
  summarise(
    mean_valor = mean(VALOR, na.rm = T),
    mean_log = mean(log_valor, na.rm = T)
  )

rubros <- unique(finance_means$DESCRIPCION_CATEGORIA)

# Plots de tendencia
for (r in rubros) {
  df_r <- finance_means %>%
    filter(DESCRIPCION_CATEGORIA == r,
           ANIO >= 2015, ANIO <= 2021)
  
  # Gráfico en niveles
  p1 <- ggplot(df_r, aes(ANIO, mean_valor)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 2015:2021, limits = c(2015, 2021)) +
    labs(
      y = "Pesos promedio"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico en logaritmos
  p2 <- ggplot(df_r, aes(ANIO, mean_log)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 2015:2021, limits = c(2015, 2021)) +
    labs(
      y = "Log(pesos promedio)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combina con patchwork y añade título
  combined <- p1 + p2 +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = str_to_title(r),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  
  print(combined)
}


# ******************************************************************************
# ---- IV. Merge of data frames ----
# ******************************************************************************

#1
incumb_fin <- reelect_panel %>% 
  left_join(
    egresos %>% 
      select(total_egresos, log_egresos,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )

#2
incumb_fin <- incumb_fin %>% 
  left_join(
    estimulos %>% 
      select(estimulos, log_estimulos,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#3
incumb_fin <- incumb_fin %>% 
  left_join(
    nomina %>% 
      select(nomina, log_nomina,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#4
incumb_fin <- incumb_fin %>% 
  left_join(
    subsidios %>% 
      select(subsidios, log_subsidios,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#5
incumb_fin <- incumb_fin %>% 
  left_join(
    obra %>% 
      select(obra, log_obra,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#6
incumb_fin <- incumb_fin %>% 
  left_join(
    impuestos %>% 
      select(impuestos, log_impuestos,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#7
incumb_fin <- incumb_fin %>% 
  left_join(
    predial %>% 
      select(predial, log_predial,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#8
incumb_fin <- incumb_fin %>% 
  left_join(
    usufructo %>% 
      select(usufructo, log_usufructo,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#9
incumb_fin <- incumb_fin %>% 
  left_join(
    permisos %>% 
      select(permisos, log_permisos,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#10
incumb_fin <- incumb_fin %>% 
  left_join(
    multas %>% 
      select(multas, log_multas,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#11
incumb_fin <- incumb_fin %>% 
  left_join(
    parti_fed %>% 
      select(parti_fed, log_parti_fed,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#12
incumb_fin <- incumb_fin %>% 
  left_join(
    parti_est %>% 
      select(parti_est, log_parti_est,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )
#13
incumb_fin <- incumb_fin %>% 
  left_join(
    aporta %>% 
      select(aporta, log_aporta,ANIO,edomun_id),
    by = c("edomun_id","year"="ANIO")
  )

incumb_fin <- incumb_fin %>% 
  mutate(ingresos_reales = total_egresos - (parti_fed + aporta),
         log_ingresos = if_else(ingresos_reales>0, log(ingresos_reales),
                                NA_real_))

# ******************************************************************************
# ---- V. Variables for DID ----
# ******************************************************************************

incumb_fin <- incumb_fin %>% 
  group_by(edomun_id) %>% 
  mutate(
    treat = as.integer(any(reelection==1 & yr == 2018))
  ) %>% 
  ungroup() %>% 
  mutate(
    post = as.integer(yr>=2018),
    did = treat*post
  )

# ******************************************************************************
# ---- VI. Parallel Trends ----
# ******************************************************************************

incumb_fin %>%
  group_by(treat, yr) %>%
  summarise(
    avg_VALOR = mean(VALOR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(yr, avg_VALOR, color = factor(treat))) +
  geom_line() +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  labs(
    color = "Reelecto",
    title = "Tendencias pre-tratamiento",
    x = "Año", y = "Monto promedio"
  ) +
  theme_minimal()

pt_if <- incumb_fin %>%
  filter(yr >= 2015, yr <= 2021) %>% 
  group_by(DESCRIPCION_CATEGORIA, treat, yr) %>%
  summarise(
    avg_valor = mean(VALOR, na.rm = TRUE),
    .groups = "drop"
  )

for(r in rubros) {
  df_r <- pt_if %>% filter(DESCRIPCION_CATEGORIA == r)
  
  p <- ggplot(df_r, aes(yr, avg_valor, color = factor(treat))) +
    geom_line(size = 1) +
    geom_vline(xintercept = 2018, linetype = "dashed") +
    scale_color_manual(values = c("0" = "tomato", "1" = "steelblue"),
                       labels = c("No reelecto", "Reelecto")) +
    scale_x_continuous(breaks = 2015:2021) +
    labs(
      title = str_to_title(r),
      x     = "Año",
      y     = "Monto promedio",
      color = "Reelecto"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# *****************************************************************************
### Test formal de parallel trends 

# 1) Filtrar sólo años < 2018
pre_df <- incumb_fin %>%
  filter(yr < 2018)

# 2) Centrar la variable tiempo para que el coeficiente sea interpretable
pre_df <- pre_df %>%
  mutate(
    year_ctr = yr - mean(yr)   # o  yr - 2018
  )
#year_ctr mide la pendiente temporal común

# 3) Regresión de tendencia
# Incluye efectos fijos de municipio para controlar niveles diferenciales
mod_pre2 <- lm(
  VALOR ~ treat * year_ctr,
  data = pre_df
)
#year_ctr mide la pendiente temporal común

# 4) Prueba de hipótesis
# SE clusterizados por municipio
se_cl2 <- vcovCL(mod_pre2, cluster = ~ pre_df$edomun_id)

# Tabla con coeficientes y p‐values
test_int <- coeftest(mod_pre2, vcov = se_cl2)["treat:year_ctr", ]
test_int
# 0.819 : se cumple el supuesto 

# *****************************************************************************
### Para cada rubro ###

# 1. Vector de mis 9 rubros (sin NA)
rubros <- incumb_fin %>%
  pull(DESCRIPCION_CATEGORIA) %>%
  unique() %>%
  na.omit()

# 2. Función que corre el test para un rubro
test_rubro <- function(r) {
  df_r <- incumb_fin %>%
    filter(DESCRIPCION_CATEGORIA == r, yr < 2018) %>%
    mutate(year_ctr = yr - mean(yr))
  
  # Si no hay suficientes datos o variación, devolvemos NA
  if(nrow(df_r) < 5 || n_distinct(df_r$VALOR) < 2) {
    return(tibble(
      rubro     = r,
      estimate  = NA_real_,
      std.error = NA_real_,
      t.value   = NA_real_,
      p.value   = NA_real_
    ))
  }
  
  # Ajusta sin FE de unidad para aislar la interacción
  mod <- lm(VALOR ~ treat * year_ctr, data = df_r)
  se  <- vcovCL(mod, cluster = ~ df_r$edomun_id)
  
  ct  <- coeftest(mod, vcov = se)["treat:year_ctr", ]
  
  tibble(
    rubro     = r,
    estimate  = ct["Estimate"],
    std.error = ct["Std. Error"],
    t.value   = ct["t value"],
    p.value   = ct["Pr(>|t|)"]
  )
}

# 3. Aplica el test a todos los rubros y junta resultados
results_tbl <- map_dfr(rubros, test_rubro)

# 4. Marca cuáles cumplen (p > 0.05) y cuáles no
results_tbl <- results_tbl %>%
  mutate(parallel = if_else(p.value > 0.05, "Sí", "No"))

print(results_tbl)

# ******************************************************************************
# ---- Parallel Trends por rubro sobre log_valor ----
# ******************************************************************************

# Filtrar rango de años
pt_df <- incumb_fin %>% 
  filter(year >= 2015, year <= 2023)

# Loop para cada variable
for (o in outcomes) {
  df_o <- pt_df %>% 
    group_by(treat, year) %>% 
    summarise(
      avg_valor = mean(.data[[o]], na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(df_o, aes(x = year, y = avg_valor, color = factor(treat))) +
    geom_line(size = 1) +
    geom_vline(xintercept = 2018, linetype = "dashed") +
    scale_color_manual(
      values = c("0" = "tomato", "1" = "steelblue"),
      labels = c("No reelecto", "Reelecto")
    ) +
    scale_x_continuous(breaks = 2015:2023) +
    labs(
      title = str_to_title(str_replace(o, "^log_", "")),  # quita el prefijo log_
      x     = "Año",
      y     = paste0("Promedio de ", o),
      color = "Reelecto"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# ******************************************************************************
# ---- VII. DID Estimator ----
# ******************************************************************************

did_mod1 <- feols(VALOR ~ did | edomun_id + yr, data = incumb_fin, cluster = ~ edomun_id)
summary(did_mod1)


# Vector de rubros
rubros <- unique(incumb_fin$DESCRIPCION_CATEGORIA) %>% na.omit()

# DID por rubro
did_models <- lapply(rubros, function(r) {
  df_r <- incumb_fin %>% 
    filter(DESCRIPCION_CATEGORIA == r)
  
  feols(
    log_valor ~ did | edomun_id + yr, # Aquí se cambia por log_valor
    data    = df_r,
    cluster = ~edomun_id
  )
})
names(did_models) <- rubros

# Resúmenes por modelo
lapply(did_models, summary)

# Extraer sólo el coeficiente “did” con sus IC y p-valor en un data.frame
did_results <- map2_df(
  did_models, rubros,
  ~ tidy(.x, conf.int = TRUE) %>% 
    filter(term == "did") %>%
    select(estimate, std.error, p.value, conf.low, conf.high) %>%
    mutate(rubro = .y)
)

#Ordena la tabla
did_results %>%
  select(rubro, everything()) %>%
  arrange(rubro)

# ******************************************************************************
### Comparando modelos ###

# Modelo en niveles
mod_niveles <- feols(VALOR ~ did | edomun_id + yr, data = incumb_fin, cluster = ~edomun_id)
# Modelo en log
mod_log     <- feols(log_valor ~ did | edomun_id + yr, data = incumb_fin, cluster = ~edomun_id)

summary(mod_niveles) # Sí salió *
summary(mod_log) # No salió sifn

# ******************************************************************************
# ---- VIII. DID Callaway & Santa'Anna ----
# ******************************************************************************

# 1. Crea la variable g para Callaway–Sant’Anna
#    Si treat==1 (reelegido en 2018) → g = 2018,  
#    si nunca se reeligió → g = 0 (nunca tratado)

cs_df <- incumb_fin %>%
  # a) convierte edomun_id de character a integer
  mutate(edomun_id = as.integer(edomun_id)) %>%
  # b) definir g 
  mutate(g = if_else(treat == 1, 2018L, 0L)) %>%
  # c) elimina filas con NA en las variables clave
  filter(!is.na(log_valor), !is.na(yr), !is.na(g), !is.na(edomun_id))

# 2. Estima ATT(g,t) con att_gt
#    - yname: variable respuesta  
#    - tname: año  
#    - idname: identificador unidad  
#    - gname: cohorte de tratamiento  
#    - control_group = "never_treated" usa los que g==0 como controles  
cs_att <- att_gt(
  yname         = "log_valor",
  tname         = "yr",
  idname        = "edomun_id",
  gname         = "g",
  data          = cs_df,
  control_group = "nevertreated",
  xformla       = ~ 1,        # sin covariables
  panel = T,  #fuerza el modo panel 
  allow_unbalanced_panel = T #permite panel no balanceado
)

# 3. Agrega efectos dinámicos (event‐study)
cs_dyn <- aggte(cs_att, type = "dynamic", na.rm = T)

# 4. Grafica event‐study con ggdid
ggdid(cs_dyn) +
  labs(
    title = "Event‐Study: efecto de reelección (Callaway & Sant’Anna)",
    x     = "Año relativo al tratamiento",
    y     = "ATT"
  )

# ******************************************************************************
### C&S por rubros ###

# Función que corre todo para un rubro
run_cs_by_rubro <- function(r) {
  df_r <- cs_df %>% filter(DESCRIPCION_CATEGORIA == r)
  
  # Si no hay datos suficientes, devolvemos NULL
  if(nrow(df_r) < 10) return(NULL)
  
  # a) att_gt
  cs_att_r <- att_gt(
    yname                  = "log_valor",
    tname                  = "yr",
    idname                 = "edomun_id",
    gname                  = "g",
    data                   = df_r,
    control_group          = "nevertreated",
    xformla                = ~ 1,
    panel                  = TRUE,
    allow_unbalanced_panel = TRUE
  )
  
  # b) event‐study
  cs_dyn_r <- aggte(cs_att_r, type = "dynamic", na.rm = TRUE)
  
  # c) plot
  p <- ggdid(cs_dyn_r) +
    labs(
      title = str_to_title(r),
      x     = "Año relativo al tratamiento",
      y     = "ATT (log-monto)"
    ) +
    theme_minimal()
  
  list(att = cs_att_r, dyn = cs_dyn_r, plot = p)
}

# Aplica a cada rubro
cs_list <- map(rubros, run_cs_by_rubro)
names(cs_list) <- rubros

# Imprime todos los gráficos
walk(cs_list, ~ {
  if(!is.null(.x)) print(.x$plot)
})

# ******************************************************************************
# ---- IX. DID Chaisemartin & D'Haultfoeuille ----
# ******************************************************************************

# 1) Peparando la base
incumb_fin <- incumb_fin %>%
  group_by(edomun_id) %>%
  mutate(
    # primer año de reelección (o 0 si nunca hubo)
    g = if_else(
      any(reelection == 1 & year >= 2018),
      min(year[reelection == 1 & year >= 2018]),
      0L
    ),
    # quiénes son tratados alguna vez
    treat = as.integer(g > 0)
  ) %>%
  ungroup() %>%
  # post = a partir de su cohorte
  mutate(
    post = as.integer(year >= g),
    did  = treat * post
  )

# 2) C&D'H 

did_cd <- did_multiplegt_dyn(
  df = incumb_fin,
  outcome = "log_multas",
  group = "edomun_id",
  time = "year",
  treatment = "reelection",
  placebo = 3,
  effects = 5,
  trends_lin = F,
  graph_off = F
)

summary(did_cd)

p <- last_plot()

# Ahora p es un objeto ggplot, así que puedes:
p1 <- p +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Event-Study: log(usufructo)",
    x     = "Años relativos a la reelección",
    y     = "ATT (log-usufructo)"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title   = element_text(size = 12),
    axis.text    = element_text(size = 10)
  )
print(p1)

# Para generar un archivo .tex con tikzDevice
library(tikzDevice)
tikz("eventstudy_egresos.tex", width = 6, height = 4, standAlone = FALSE)
print(p1)
dev.off()


# 3) Para ver p-value por estimate
es_tab <- did_cd$results$Effects
es_tab2 <- es_tab %>%
  as.data.frame() %>%                  
  rownames_to_column(var = "term") %>% 
  as_tibble() %>%                      
  mutate(
    p.value = 2 * (1 - pnorm(abs(Estimate / SE)))
  )

print(es_tab2)

## 4) Para la interpretación: Delta%(k) = 100[e^Bk-1]

effects <- es_tab
pct_change <- 100*(exp(effects)-1)
pct_change

# ******************************************************************************
# C&D'H por rubros 

outcomes <- c(
  "log_egresos","log_estimulos","log_nomina","log_subsidios","log_obra",
  "log_impuestos","log_predial","log_usufructo","log_permisos","log_multas",
  "log_parti_fed","log_aporta","log_ingresos"
)

# Reserva una lista para los resultados
did_results <- vector("list", length(outcomes))
names(did_results) <- outcomes

# Loop sobre cada outcome
for(o in outcomes) {
  did_results[[o]] <- did_multiplegt_dyn(
    df         = incumb_fin,
    outcome    = o,
    group      = "edomun_id",
    time       = "year",
    treatment  = "reelection",       
    placebo    = 3,
    effects    = 5,
    trends_lin =  F,
    graph_off  = TRUE
  )
}

# Para ver el resumen de uno:
summary(did_results[["log_multas"]])

# Todos a la vez:
lapply(did_results, summary)

# Para cada modelo extraemos p-valores de efectos y placebos
resumen <- map_dfr(outcomes, function(o) {
  mod <- did_results[[o]]
  # summary(mod) imprime, pero no devuelve. En cambio:
  e <- mod$effects        # tabla con Estimate, SE, etc.
  p_eff <- mod$results$test_joint_eff$p.value
  p_pl  <- mod$results$test_joint_pl$p.value
  
  tibble(
    outcome           = o,
    pval_effects      = p_eff,
    pval_placebos     = p_pl,
    signif_effects    = p_eff < 0.05,
    parallel_trends   = p_pl  > 0.05
  )
})

print(resumen)

# ******************************************************************************
# ---- X. Synthetic Control ----
# ******************************************************************************

### General ###
gsynth_out <- gsynth(
  formula    = log_ingresos ~ reelection,  # Outcome ~ Treatment + covariables
  data       = incumb_fin,                         # data.frame
  index      = c("edomun_id", "year"),             # identificador de panel
  force      = "two-way",                          # unidad + tiempo
  r          = c(0, 5),                            # rango de factores latentes
  CV         = TRUE,                               # usar validación cruzada
  criterion  = "mspe",                             # criterio de CV
  se         = TRUE,                               # calcular errores estándar
  nboots     = 200,                                # número de réplicas bootstrap
  inference  = "nonparametric",                    # inferencia no paramétrica
  na.rm = TRUE,
  min.T0 = 7,
  parallel   = TRUE                                # correr en paralelo si es posible
)

print(gsynth_out)

# ******************************************************************************
### Por rubro ###

run_gsynth <- function(y) {
  message("Corriendo gsynth para: ", y)
  gsynth(
    formula   = as.formula(paste0(y, " ~ reelection")),
    data      = incumb_fin,
    index     = c("edomun_id", "year"),
    force     = "two-way",
    r         = c(0, 5),
    CV        = TRUE,
    criterion = "mspe",
    se        = TRUE,
    nboots    = 200,
    inference = "nonparametric",
    na.rm     = TRUE,
    min.T0    = 7,
    parallel  = TRUE,
    seed      = 2025
  )
}

gsynth_results <- map(outcomes, run_gsynth)
names(gsynth_results) <- outcomes


# ******************************************************************************
### Plots ###

## Contrafactual vs Real 

# 1.1 Extraer los datos de contrafactual (Y.ct) y real (Y.tr)
# Para cada unidad tratada y cada año de tu panel:
df_cf <- map_dfr(
  seq_along(gsynth_out$id.tr),
  function(i) {
    tibble(
      muni_id = gsynth_out$id.tr[i],      # scalar, se recicla 9 veces
      year    = gsynth_out$time,          # length 9
      real    = gsynth_out$Y.tr[, i],     # extrae la COLUMNA i → length 9
      synth   = gsynth_out$Y.ct[, i]      # idem para la sintética
    )
  }
)

# Comprueba
df_cf %>% 
  group_by(muni_id) %>% 
  summarise(n = n())   # debe salir 9 para cada muni_id

# Pasar a formato largo
df_cf_long <- df_cf %>%
  pivot_longer(c(real, synth), names_to = "tipo", values_to = "valor")

df_mean <- df_cf_long %>%
  group_by(year, tipo) %>%
  summarise(mean_val = mean(valor, na.rm = TRUE), .groups = "drop")

# Graficar la media real vs. sintética
ggplot(df_mean, aes(x = year, y = mean_val, color = tipo)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  labs(
    title = "Media real vs sintética (municipios tratados)",
    x     = "Año",
    y     = "Promedio log_egresos",
    color = ""
  ) +
  theme_minimal()

# 1.2 Graficar (ejemplo para un municipio; filtra muni_id de interés)
muni_sel <- df_cf$muni_id[1]  # o reemplaza por el ID que quieras
df_cf %>%
  filter(muni_id == muni_sel) %>%
  pivot_longer(c(real, synth), names_to = "tipo", values_to = "valor") %>%
  ggplot(aes(x = year, y = valor, color = tipo)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  labs(
    title = paste("Contrafactual vs Real para", muni_sel),
    x = "Año", y = "log_egresos",
    color = ""
  ) +
  theme_minimal()

## Average ATT
# 2.1 Extraer años y ATT promedio por año
df_att <- tibble(
  year = gsynth_out$time,
  att  = gsynth_out$att
)

# 2.2 Graficar
df_att %>%
  ggplot(aes(x = year, y = att)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  labs(
    title = "Average Treatment Effect on the Treated",
    x = "Año", y = "ATT (log_egresos)"
  ) +
  theme_minimal()

## Efectos Dinámicos 

# 3.1 Extraer efectos año a año (gsynth_out$eff)
#    and pre-post flags
df_dyn <- tibble(
  year   = gsynth_out$time,
  effect = gsynth_out$att
)

# 3.2 Graficar event‐study
ggplot(df_dyn, aes(x = year, y = effect)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "red") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Event‐Study: Efectos dinámicos post‐reelección",
    subtitle = "Coeficiente del ATT en cada año",
    x = "Año", 
    y = "ATT (log_egresos)"
  ) +
  theme_minimal()

# ******************************************************************************
# ---- XI. Changes in Changes ----
# ******************************************************************************

incumb_fin <- incumb_fin %>%
  group_by(edomun_id) %>%
  mutate(
    g = if (any(reelection == 1 & year >= 2018)) {
      min(year[reelection == 1 & year >= 2018], na.rm = TRUE)
    } else {
      0L
    }
  ) %>%
  ungroup()

df_ecic <- incumb_fin %>%
  filter(
    !is.na(log_obra),
    is.finite(log_obra),
    g >= 0L,
    !is.na(year)
  )

res_ecic <- ecic(
  yvar   = log_obra,  # outcome
  gvar   = g,             # cohorte: año de entrada
  tvar   = year,          # variable de tiempo
  ivar   = edomun_id,     # ID municipio
  dat    = df_ecic,
  es     = TRUE,          # event‐study dinámico
  boot   = "weighted",    # bootstrap
  nReps  = 200
)

summary(res_ecic)

### Plot ###
tab_ecic <- summary(res_ecic)
q <- ecic_plot(tab_ecic)
print(q)

tab <- map_dfr(tab_ecic, ~ as_tibble(.x))
ggplot(tab, aes(x = es, y = coefs)) +

    geom_line() +
  geom_ribbon(aes(
    ymin = coefs - 1.96 * se,
    ymax = coefs + 1.96 * se
  ), alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ perc, scales = "free_y") +
  scale_x_continuous(breaks = unique(tab$es)) +
  labs(
    title = "Event-Study CiC por percentil",
    x     = "Años relativos al tratamiento",
    y     = "Quantile Treatment Effect"
  ) +
  theme_minimal(base_family = "serif")

### Ver significacncia ###

# Combínalas en un solo tibble y añade el nombre del período es
lst <- imap_dfr(tab_ecic, ~ as_tibble(.x) %>% mutate(es = as.numeric(.y)))

# Calcula t y p‐value
tab2 <- tab %>%
  mutate(
    t_stat  = coefs / se,
    p.value = 2 * (1 - pnorm(abs(t_stat)))
  )

# Echa un vistazo completo
print(tab2)

# Filtra los significativos (p < 0.05)
sig <- tab2 %>% filter(p.value < 0.05)
print("Coeficientes significativos (p < 0.05):")
print(sig)

