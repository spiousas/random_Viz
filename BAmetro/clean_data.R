library(janitor)
library(tidyverse)

# 2020 ####
pasajeros_estaciones_2020_raw <- read_csv("./BAmetro/data/historico2020.csv") %>%
  clean_names() 

pasajeros_estaciones_2020_daily <- pasajeros_estaciones_2020_raw %>% 
  drop_na() %>%
  mutate(fecha_aux = dmy(fecha),
         fecha = if_else(is.na(fecha_aux), mdy(fecha), fecha_aux),
         day = day(fecha),
         month = month(fecha),
         week = week(fecha),
         linea = str_sub(linea, -1),
         estacion = paste0("linea ", linea,  " - ", tolower(estacion)),
         estacion = case_when(estacion == "linea H - once" ~ "linea H - once/30 de diciembre",
                              estacion == "linea B - callao.b" ~ "linea B - callao",
                              TRUE ~ estacion)) %>%
  select(-fecha_aux) %>%
  group_by(fecha, day, month, week, estacion, linea) %>%
  summarise(pasajeros = sum(pax_total))

pasajeros_estaciones_2020_daily %>% write_csv("./BAmetro/data/historico2020_daily.csv")

# 2019 ####
pasajeros_estaciones_2019_raw <- read_csv("./BAmetro/data/historico2019.csv") %>%
  clean_names() 

pasajeros_estaciones_2019_daily <- pasajeros_estaciones_2019_raw %>% 
  drop_na() %>%
  mutate(fecha = ymd(fecha),
         day = day(fecha),
         month = month(fecha),
         week = week(fecha),
         linea = str_sub(linea, -1),
         estacion = paste0("linea ", linea,  " - ", tolower(estacion)),
         estacion = case_when(estacion == "linea H - once" ~ "linea H - once/30 de diciembre",
                              estacion == "linea B - callao.b" ~ "linea B - callao",
                              TRUE ~ estacion)) %>%
  group_by(fecha, day, month, week, estacion, linea) %>%
  summarise(pasajeros = sum(total))

pasajeros_estaciones_2019_daily %>% write_csv("./BAmetro/data/historico2019_daily.csv")
