#############################################
# S8 Hypothesis 1: Store level forecast EDA #
#############################################
# Fecha: 2023-09-11
# Creador: FR
# La idea de fondo es reducir la complejidad del forecasting a nivel
# store-family y reducirlo a solo store level. En total serían 54 forecasteo. 
# Se hara posteriormente un top-down approach para las familias, para eso 
# hay que investigar que tan volatil son los % de ventas del total del store por
# familia. Si esta volatilidad es baja entonces es posible forecastear con tranquilidad
# y tener resultados aceptables. 

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
# sc <- SparkR::sparkR.session(master="local")
datawrang_libs() # cargando librerias de datawrangling
ml_libs() # cargando librarias para ML
library(plotly)
library(slider)

# Data --------------------------------------------------------------------
df_list <- read_rds("Data/df_list_kgl.RDS")
train <- df_list$train
transactions <- df_list$transactions
oil <- df_list$oil
holidays <- df_list$holidays_events
stores <- df_list$stores
test <- df_list$test

# Hypothesis 1 ------------------------------------------------------------
# Is store level forecasting useful for a posterior top-down approach to sales?
# Obtengamos los % de ventas de cada familia y para el ejercico 
# usares store_nbr == 1
# Miraremos la volatilidad por bisemana (similar al forecasting)
df_str1 <- train %>% 
  select(-onpromotion) %>% 
  # Generando ventas totales por store
  group_by(date, store_nbr) %>% 
  mutate(sales_total = sum(sales)) %>% 
  ungroup() %>% 
  filter(store_nbr == 1) %>% # analizemos solo 1 store
  arrange(date) %>% 
  # Generando otros features
  mutate(pct = sales/sales_total*100, 
         pct_rm_7d = slide_dbl(pct, mean, .before = 6, .after = 0, .complete = TRUE), 
         week_num = isoweek(date),
         biweek_num = ceiling(week_num / 2),
         # generamos la marca de bisemanal anual
         yearbiweek = paste(year(date), biweek_num, sep = "_")) %>% 
  group_by(yearbiweek) %>% 
  mutate(datebiweek = min(date)) %>% # fecha inicial de cada semana
  ungroup()

# Se observa estabilidad anual, algunos años cambia la tendencia entre años
(df_str1 %>% 
    ggplot(aes(x = date, y = pct_rm_7d)) +
    geom_line() +
    facet_wrap(~family) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))) %>% 
  ggplotly()

# Revisemos la volatilidad a nivel de semana
df_str1_biweek <- df_str1 %>% 
  group_by(datebiweek, family) %>% 
  summarise(sales_week = sum(sales),
            pct_mean = mean(pct),
            pct_max = max(pct),
            pct_min = min(pct),
            pct_sd = sd(pct)) %>% 
  ungroup() %>% 
  mutate(coef_var = pct_sd/pct_mean)

# Se ve relativamente estable intrasemana, salvo unas semanas en el 2013
# tenemos un corte en entre Junio 24 y Julio 8 2013
plot_ly(data = df_str1_biweek, 
        x = ~datebiweek, 
        y = ~pct_mean, 
        color = ~family,
        type = 'scatter', 
        mode = 'lines+markers',
        error_y = ~list(array = pct_sd, color = '#000000'))

# veamos las semanas y productos más volatiles
# La gran mayoría tiene variación menos de 1%
# Productos más volátiles:
# PRODUCE - 137 bisemanas con variación 2-5% y 4 semanas con más de 10%
# MEATS - 17 bisemanas con variación 1-2% 
# GROCERY I - 7 bisemanas 5-10%
# BEVERAGES - 25 bisemanas 2-5%
# CLEANING - 9 bisemanas  2-5%
df_str1_biweek %>% 
  mutate(var_desc = case_when(coef_var < 0.1 ~ "menor 10%",
                              coef_var < 0.2 ~ "menor 20%",
                              coef_var < 0.5 ~ "menor 50%", 
                              coef_var < 1 ~ "menor 100%", 
                              TRUE ~ "mayor 1000%"),
         var_desc = factor(var_desc, level = c("menor 1%", "menor 20%", "menor 50%", 
                                               "menor 100%", "mayor 100%"))) %>% 
  na.omit() %>% 
  count(family, var_desc) %>% 
  spread(var_desc, n, fill = 0) %>% 
  fun_print()

# Hasta ahora se viable asumir que los porcentajes son relativamente estables
# para este store son pocos que experimentan variaciones sobre 5% dentro de una 
# bisemana. Ahora vamos a escalar esto a todos los stores y veremos los complicados.
df_strs <- train %>% 
  select(-onpromotion) %>%
  arrange(date) %>% 
  # Generando ventas totales por store 
  group_by(date, store_nbr) %>% 
  mutate(sales_total = sum(sales)) %>% 
  group_by(store_nbr) %>% 
  # Generando otros features
  mutate(pct = sales/sales_total * 100, 
         pct_rm_7d = slide_dbl(pct, mean, .before = 6, .after = 0, .complete = TRUE), 
         week_num = isoweek(date),
         biweek_num = ceiling(week_num / 2),
         # generamos la marca de bisemanal anual
         yearbiweek = paste(year(date), biweek_num, sep = "_")) %>% 
  ungroup() %>% 
  group_by(yearbiweek) %>% 
  mutate(datebiweek = min(date)) %>% # fecha inicial de cada semana
  ungroup()

# Variación global --------------------------------------------------------
# Revisemos la volatilidad a nivel de bisemana por store
df_strs_biweek <- df_strs %>% 
  group_by(datebiweek, store_nbr, family) %>% 
  summarise(sales_week = sum(sales),
            pct_mean = mean(pct),
            pct_max = max(pct),
            pct_min = min(pct),
            pct_sd = sd(pct)) %>% 
  ungroup() %>% 
  mutate(coef_var = pct_sd/pct_mean)

# Variación global
# Un 30% de las bisemanas de todo el train, el porcentaje de 
# variación de las familias es menor a 20%. 
# Por el contrario un 8% tiene variación sobre entre 80-100% intra bisemana. 
df_strs_biweek %>% 
  mutate(var_desc = case_when(coef_var < 0.1 ~ "menor 10%",
                              coef_var < 0.2 ~ "menor 20%",
                              coef_var < 0.5 ~ "menor 50%", 
                              coef_var < 0.8 ~ "menor 80%",
                              coef_var < 1 ~ "menor 100%", 
                              TRUE ~ "mayor 1000%"),
         var_desc = factor(var_desc, level = c("menor 1%", "menor 20%", "menor 50%", "menor 80%",
                                               "menor 100%", "mayor 100%"))) %>% 
  na.omit() %>% 
  count(store_nbr,var_desc) %>% 
  group_by(var_desc) %>% 
  summarise(n_prom = round(mean(n))) %>% 
  mutate(pct = round(n_prom/sum(n_prom)*100,1)) %>% 
  knitr::kable() # to readme

# Conclusión, la variabilidad intra dos semanas no es menor
# sin embargo, todo es menor a 100% por lo que podría servir esta hipótesis
# habría que probar distintas ventanas

# Variación Nivel store ---------------------------------------------------
# Hay que tener ojo con las siguientes tiendas:
# store_nbr = 10
# store_nbr = 26
# store_nbr = 13
df_strs_biweek %>% 
  mutate(var_desc = case_when(coef_var < 0.1 ~ "menor 10%",
                              coef_var < 0.2 ~ "menor 20%",
                              coef_var < 0.5 ~ "menor 50%", 
                              coef_var < 0.8 ~ "menor 80%",
                              coef_var < 1 ~ "menor 100%", 
                              TRUE ~ "mayor 1000%"),
         var_desc = factor(var_desc, level = c("menor 1%", "menor 20%", "menor 50%", "menor 80%",
                                               "menor 100%", "mayor 100%"))) %>% 
  na.omit() %>% 
  count(store_nbr, var_desc) %>% 
  spread(var_desc, n, fill = 0) %>% 
  arrange(desc(`menor 100%`)) %>% 
  fun_print()

# Revisemos el producto problemático
# Para el store 23 es MEATS
# GROCERY I es problematico para 26, 32
# PRODUCE en 24 también.
df_strs_biweek %>% 
  filter(store_nbr %in% c(23,32,26,24,30)) %>% 
  mutate(var_desc = case_when(pct_sd < 0.01 ~ "menor 1%",
                              pct_sd < 0.02 ~ "menor 2%",
                              pct_sd < 0.05 ~ "menor 5%", 
                              pct_sd < 0.1 ~ "menor 10%", 
                              TRUE ~ "mayor 10%"),
         var_desc = factor(var_desc, level = c("menor 1%", "menor 2%", "menor 5%", 
                                               "menor 10%", "mayor 10%"))) %>% 
  na.omit() %>% 
  count(store_nbr, family, var_desc) %>% 
  spread(var_desc, n, fill = 0) %>% 
  arrange(desc(`mayor 10%`))
