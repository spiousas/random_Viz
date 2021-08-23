pacman::p_load(BBmisc, tidyverse, hablar, ggbump, sf, rnaturalearth, feather, janitor, lubridate,
               colorspace, ggtext, ggbump, extrafont, lubridate, cowplot, here)

pasajeros_estaciones<- read_csv(here("./BAmetro/data/historico2019_daily.csv")) %>%
  group_by(estacion) %>%
  summarise(pasajeros_2019 = sum(pasajeros)) %>%
  ungroup() %>%
  slice_max(pasajeros_2019, n = 20) %>%
  left_join(read_csv("./BAmetro/data/historico2020_daily.csv") %>%
              group_by(estacion) %>%
              summarise(pasajeros_2020 = sum(pasajeros))) %>%
  mutate(disminucion = (pasajeros_2019-pasajeros_2020)/pasajeros_2019 * 100)

caba <- st_read(here("./BAmetro/data/barrios/barrios_badata.shp")) %>%
  clean_names()

subte <- st_read(here("./BAmetro/data/lineas-de-subte/lineas-subte.shp")) %>%
  clean_names() %>%
  mutate(linea = str_sub(lineasub,-1))

estaciones <- st_read(here("./BAmetro/data/estaciones-de-subte/estaciones-de-subte.shp")) %>%
  clean_names() %>%
  mutate(linea = str_sub(linea, -1),
         estacion = paste0("linea ", linea,  " - ", tolower(estacion)),
         estacion = case_when(estacion == "linea B - juan manuel de rosas - villa urquiza" ~ "linea B - rosas",
                              estacion == "linea B - c. pellegrini" ~ "linea B - carlos pellegrini",
                              estacion == "linea B - malabia - osvaldo pugliese" ~ "linea B - malabia",
                              estacion == "linea B - almagro - medrano" ~ "linea B - medrano",
                              estacion == "linea H - once - 30 de diciembre" ~ "linea H - once/30 de diciembre",
                              estacion == "linea B - callao - maestro alfredo bravo"  ~ "linea B - callao",
                              TRUE ~ estacion))

estaciones_filtered <- estaciones %>%
  inner_join(pasajeros_estaciones, by = "estacion") %>%
  rowwise() %>%
  mutate(estacion_label = str_split(estacion, " - ")[[1]][2])

estaciones_ranking <- st_geometry(estaciones_filtered) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(pasajeros_cap = normalize(rank(estaciones_filtered$pasajeros_2019), range = c(-34.704510, -34.531899), method = "range"),
                   estacion = estaciones_filtered$estacion,
                   estacion_label = estaciones_filtered$estacion_label,
                   linea = estaciones_filtered$linea,
                   xend = -58.27,
                   x_axis_start = xend + .01,
                   pasajeros_cap_x_2019 = first(x_axis_start) + diff(c(first(x_axis_start), -58)) * estaciones_filtered$pasajeros_2019 / max(estaciones_filtered$pasajeros_2019),#normalize(estaciones_filtered$pasajeros_2019, range = c(first(x_axis_start), -58), method = "range"),
                   pasajeros_cap_x_2020 = first(x_axis_start) + diff(c(first(x_axis_start), -58)) * estaciones_filtered$pasajeros_2020 / max(estaciones_filtered$pasajeros_2019),
                   x_axis_start_disminucion = pasajeros_cap_x_2019 - (pasajeros_cap_x_2019-x_axis_start) * estaciones_filtered$disminucion/100 + 0.012,
                   pasajeros_cap_x_disminucion = pasajeros_cap_x_2019,
                   val_txt_aux = paste0(round(estaciones_filtered$pasajeros_2019/1e6, digits = 2), "M"),
                   val_txt_2019 = as.character(if_else(estacion == "linea C - constitucion", paste0("2019: ", val_txt_aux, " pasajeros"), val_txt_aux)),
                   val_txt_2020 = paste0(round(estaciones_filtered$pasajeros_2020/1e6, digits = 2), "M"),
                   val_txt_aux2 = paste0(round(estaciones_filtered$disminucion), "%"),
                   val_txt_disminucion = as.character(if_else(estacion == "linea C - constitucion", paste0("2020: ", val_txt_aux2, " menos"), val_txt_aux2)))) %>%
  select(-c(val_txt_aux, val_txt_aux2))


cols_subte <- lighten(c("#49B1DE", "#E3492D", "#0E6AAC", "#007864", "#7B1D82", "#FCD500"), 
                      amount = 0.4)

estaciones_filtered <- estaciones_filtered %>% 
  bind_cols(estaciones_ranking %>% select(pasajeros_cap))

fig_mapa <- ggplot() + 
  # Mapa de la ciudad con las comunas
  geom_sf(data = st_transform(caba, "+proj=longlat +datum=WGS84"),
          size = .3,
          fill = "transparent",
          color = "gray70") +
  # Mapa de las líneas de subte
  geom_sf(data = subte, aes(color = linea),
          size = 1,
          fill = "transparent",
          alpha = 0.4) +
  # Mapa de las estaiones de subte
  geom_sf(data = estaciones, aes(color = linea),
          size = 2,
          fill = "transparent") +
  # Mapa Sigmoidea que uno estaciones con el comienzo de las barras
  geom_sigmoid(data = estaciones_ranking, 
               aes(x = X, y = Y, xend = x_axis_start - .001, yend = pasajeros_cap, group = estacion, color = linea), 
               alpha = .6, smooth = 10, size = 1) +
  # Barras de 2019
  geom_segment(data = estaciones_ranking, 
               aes(x = pasajeros_cap_x_2019, y = pasajeros_cap, xend = x_axis_start , yend = pasajeros_cap, color = linea), alpha = .6, size = 1, 
               lineend = "round") +
  # Barras de 2020
  geom_segment(data = estaciones_ranking, 
               aes(x = pasajeros_cap_x_2020, y = pasajeros_cap -  0.0015, xend = x_axis_start , yend = pasajeros_cap -  0.0015), color = "gray90", alpha = .6, size = 1, 
               lineend = "round") +
  # Barra para emprolijar el inicio de las barras
  geom_segment(data = estaciones_ranking, 
               aes(x = x_axis_start, y = -34.71, xend = x_axis_start, yend = -34.53), alpha = .6, size = 1, color = "#605D5C") +
  # Texto nombre de estaciones
  geom_text(data = estaciones_ranking, aes(x = x_axis_start - .002, y = pasajeros_cap, label = estacion_label, color = linea), hjust = 1, size = 3, nudge_y = 0.003) +
  # Texto nombre pasajeros 2019
  geom_text(data = estaciones_ranking, aes(x = pasajeros_cap_x_2019, y = pasajeros_cap, label = val_txt_2019, color = linea), hjust = 0, vjust = 0, size = 3, nudge_x = .002) +
  # Texto nombre pasajeros 2020
  geom_text(data = estaciones_ranking, aes(x = pasajeros_cap_x_2020, y = pasajeros_cap - 0.0015, label = val_txt_2020), color = "gray90", hjust = 0, vjust = 1, size = 3, nudge_x = .002) +
  # Texto nombre pasajeros disminucion
  # geom_text(data = estaciones_ranking, aes(x = pasajeros_cap_x_disminucion, y = pasajeros_cap - 0.0015, label = val_txt_disminucion), color = "gray70", hjust = 0, vjust = 1, size = 2.5, nudge_x = .002) +
  coord_sf(clip = "off") +
  scale_colour_manual(values = cols_subte) +
  theme_void() +
  labs(title = "Disminución de los viajes en subte debido al ASPO",
       subtitle = str_wrap("En colores podemos ver las 20 estaciones con más pasajeros en 2019 
                           mientras que en gris vemos el mismo período para estas estaciones pero en el años 2020 
                           junto con su disminución porcentual en la cantidad de pasajeros", 110),
       caption = "**Source:** BA Data | **Viz:** @spiousas") + 
  theme(plot.margin = margin(.5, 1.8, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "#605D5C", color = "#605D5C"),
        plot.title = element_text(color = "white", size = 16, family = "Roboto", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "white", size = 10, family = "Roboto"),
        plot.caption = element_markdown(color = "white", family = "Roboto"),
        plot.caption.position = "plot") 

 # Tiempo ####
pasajeros_estaciones_diario <- read_csv("./BAmetro/data/historico2020_daily.csv") %>%
  filter(week<53) %>%
  group_by(week) %>%
  summarise(pasajeros_2020_semanal = sum(pasajeros),
            fecha = mean(fecha)) %>%
  left_join(read_csv("./BAmetro/data/historico2019_daily.csv") %>%
              filter(week<53) %>%
               group_by(week) %>%
               summarise(pasajeros_2019_semanal = sum(pasajeros)))

labels <- tibble(week = 12,
                 label = c("Comienzo del ASPO")) 

annotations <- inner_join(pasajeros_estaciones_diario, labels)

fig_diario <- ggplot() + 
  geom_area(data = pasajeros_estaciones_diario, 
            aes(x = fecha, y = pasajeros_2019_semanal),
            color = "gray50",
            fill = "gray50") +
  geom_area(data = pasajeros_estaciones_diario, 
            aes(x = fecha, y = pasajeros_2020_semanal),
            color = "gray70",
            fill = "gray70") +
  geom_segment(data = annotations, aes(x = fecha, xend = fecha, y = 0, yend = pasajeros_2019_semanal),
               color =  lighten("#E3492D", amount = 0.2), linetype = "dashed", size = 1) +
  geom_richtext(data = annotations, aes(x = fecha+5, y = pasajeros_2019_semanal/2), label = "**20/03/2020**<br>Comienzo del ASPO",
            color =  "gray95", linetype = "dashed", size = 3, hjust = 0, fill = NA) +
  coord_cartesian(expand = TRUE) +
  scale_y_continuous(breaks = c(0, 2e6, 4e6, 6e6),
                     labels = c("0M", "2M", "4M", "6M")) +
  scale_x_continuous(breaks = ymd(c("2020-01-15", "2020-02-15", "2020-03-15", "2020-04-15", "2020-05-15", "2020-06-15",
                                    "2020-07-15", "2020-08-15", "2020-09-15", "2020-10-15", "2020-11-15", "2020-12-15")),
                     labels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")) +
  theme_void() +
  labs(title = "Pasajeros totales por semana") +
  theme(axis.text.y = element_markdown(color = "gray90", family = "Roboto", size = 10, hjust = 1, vjust = 0),
        axis.text.x = element_markdown(color = "gray90", family = "Roboto", size = 10, hjust = .5, vjust = 1),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "#605D5C", colour = "#605D5C"),
        plot.title = element_text(color = "white", size = 12, family = "Roboto", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "white", size = 10, family = "Roboto"),
        plot.caption = element_markdown(color = "white", family = "Roboto"),
        plot.caption.position = "plot")

fig_total <- fig_mapa + draw_plot(fig_diario, x = -58.15, y = -34.712, width = 0.18,
                                  height = 0.09, scale = 1, hjust = 0, vjust = 0) 

fig_total

ggsave("./BAmetro/output/comparativa_2019_2020.png", dpi = 600, width = 38, units = "cm")
