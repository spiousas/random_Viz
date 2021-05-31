library(tidyverse)
library(ggtext)
library(ggpubr)
library(ggthemes)
library(cowplot)
library(countrycode)
library(colorspace)
library(patchwork)
library(ggrepel)
library(sf)
library(maptools)
library(cartogram)
library(patchwork)
library(rcartocolor)

covid_tbl <- read_csv("./Covid_vaccines/data/owid-covid-data.csv")

vaccines_tbl <- covid_tbl %>%
  filter(str_length(iso_code)==3 & population>1e6) %>%
  drop_na(people_fully_vaccinated) %>%
  group_by(location) %>%
  slice_max(date, n=1) %>%
  mutate(second_per_100_first = people_fully_vaccinated/people_vaccinated * 100,
         continent = if_else(location=="Argentina", "Argentina", continent),
         continent = recode(continent, 
                            Europe = "Europa",
                            `North America` = "América del Norte",
                            `South America` = "América del Sur",
                            Africa = "África"),
         location = countrycode(location, origin = 'country.name', 
                                destination = 'un.name.es',
                                custom_match = c('Hong Kong' = 'Hong Kong', 
                                                 'Palestine' = 'Palestina',
                                                 'United Kingdom' = 'Reino Unido',
                                                 'Bolivia' = 'Bolivia',
                                                 'Laos' = 'Laos',
                                                 'United States' = 'Estados Unidos',
                                                 'Timor' = 'Timor'))) %>%
  select(iso_code, continent, date, gdp_per_capita, location, people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred, second_per_100_first, population, population_density) %>%
  rename(ISO3 = iso_code)

# Map ####
data("wrld_simpl")

sf_vaccinated <- 
  wrld_simpl %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=robin") %>% 
  mutate(country_code = as.character(ISO3)) %>% 
  left_join(vaccines_tbl)

base_map <-
  ggplot() +
  rcartocolor::scale_fill_carto_c(palette = "BluYl", 
                                  direction = -1) +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) 

sf_vaccinated_carto <- cartogram_cont(x = sf_vaccinated, 
                                      weight = "people_vaccinated_per_hundred", 
                                      itermax = 7)

sf_GDP_carto <- cartogram_cont(x = sf_vaccinated, 
                               weight = "gdp_per_capita", 
                               itermax = 7)

map_vaccinated <- 
  base_map +
  geom_sf(data = sf_vaccinated_carto, 
          aes(geometry = geometry, 
              fill = people_vaccinated_per_hundred),
          color = "transparent", size = 0.1) +
  labs(subtitle = "**Vaccines** per 100 people") +
  theme(legend.position = "bottom") +
  theme_fivethirtyeight() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5, family = "Helvetica"),
        legend.position = "none")
map_vaccinated

map_GDP <- 
  base_map +
  geom_sf(data = sf_GDP_carto, 
          aes(geometry = geometry, 
              fill = log10(gdp_per_capita)),
          color = "transparent", 
          size = 0.1) +
  labs(subtitle = "**GDP** per capita") +
  theme_fivethirtyeight() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5, family = "Helvetica"),
        legend.position = "none")
map_GDP

inset_map <- (map_vaccinated / map_GDP) +
  plot_annotation(theme = theme(plot.background = element_rect(color = 'gray60', size = 2, fill ="#F0F0F0")))

# Correlation ####
gdp_vs_vaccines <- vaccines_tbl %>% 
  ggplot(aes(x = log10(gdp_per_capita),
             y = people_vaccinated_per_hundred,
             label = location,
             color = continent,
             fill = continent)) +
  geom_smooth(aes(color = NA, 
                  fill = NA),
              color = "gray30",
              method='lm', 
              formula= y~exp(x)) +
  coord_cartesian(expand = TRUE) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 70)) +
  geom_label_repel(data = vaccines_tbl %>% filter(continent %in% c("América del Sur", "Argentina")),
                   box.padding = 0.4,
                   max.overlaps = Inf,
                   color = "black",
                   fill = "gray") +
  scale_fill_manual(values = c(rep("gray60",3), "#3C537A", rep("gray60"), "#3C537A", rep("gray60")),
                    breaks = c(unique(vaccines_tbl$continent)),
                    name = NULL) +
  scale_color_manual(values = c(rep("gray60",3), darken("#3C537A"), rep("gray60"), "#3C537A", rep("gray60")),
                     breaks = c(unique(vaccines_tbl$continent)),
                     name = NULL) +
  labs(title = "Vaccines and GDP",
       subtitle = "Maps are aberrated according to the plotted magnitude (**vaccines** per hundred people and **GDP** per capita).",
       x = "log10(**GDP** per capita)",
       y = "**Vaccines** per 100 people",
       caption = "<br/>**Source:** Mathieu, E., Ritchie, H., Ortiz-Ospina, E. et al. A global database of COVID-19 vaccinations.Nat Hum Behav (2021)<br/>
                  **Viz:** @spiousas") +
  theme_fivethirtyeight() +
  theme(plot.caption.position = 'plot',
        plot.title.position = 'plot',
        axis.title.x = element_markdown(size = 12, family = "Helvetica"),
        axis.title.y = element_markdown(size = 12, family = "Helvetica"),
        axis.text = element_markdown(family = "Helvetica"),
        plot.caption = element_markdown(family = "Helvetica"),
        plot.subtitle = element_markdown(size = 12, family = "Helvetica"),
        legend.position = "none") +
  draw_plot(inset_map,
            x = 2.9,
            y = 22,
            width = 1,
            height = 48,
            scale = 1,
            hjust = 0,
            vjust = 0,
            halign = 0.5,
            valign = 0.5)
gdp_vs_vaccines

ggsave("./Covid_vaccines/output/vaccine_GDP.png", dpi = 300, width = 22, height = 18, units = "cm")
