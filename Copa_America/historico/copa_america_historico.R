library(rvest)
library(tidyverse)
library(ggflags)
library(countrycode)
library(janitor)
library(ggtext)
library(extrafont)
library(colorspace)
library(paletteer)

wiki <- "https://en.wikipedia.org/wiki/Copa_América"

rank_coding <- c("winners" = 1, "runners_up" = 2, "third_place" = 3, "fourth_place" = 4)

data_CSera = wiki %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') %>%
  html_table(fill = TRUE) %>%
  clean_names() %>%
  rename("score_2" = "score",
         "score" = "score_and_venue") %>%
  mutate(tournament = "Campeonato Sudamericano")

data_CAera = wiki %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
  html_table(fill = TRUE) %>%
  clean_names() %>%
  mutate(tournament = "Copa América")

#data_total <- rbind(data_CSera, data_CAera) %>%
# Solo copa América
data_total <- data_CAera %>%
  select(!starts_with("x") & !starts_with("score")) %>%
  pivot_longer(
    cols = c("winners", "runners_up", "third_place", "fourth_place"),
    names_to = "rank",
    values_to = "country",
    values_drop_na = TRUE) %>%
  filter(year<2022) %>%
  mutate(year = parse_number(year),
         rank = as.numeric(recode(rank, !!!rank_coding)),
         is_host = if_else(country==hosts, 1, 0),
         country_id = countrycode(country,
                                  origin = "country.name",
                                  destination = "genc2c") %>% tolower())
  
pal <- paletteer_dynamic("cartography::pastel.pal", length(unique(data_total$country)))
col <- "#3C537A"
medals <- c("gold", "#C0C0C0", "#cd7f32")

Tournament_id <- c("Copa América")

lines <- tibble(year = rep(data_total %>% filter(tournament %in% Tournament_id) %>% pull(year) %>% unique(), 2),
                rank = c(rep(1, length(data_total %>% filter(tournament %in% Tournament_id) %>% pull(year) %>% unique())),
                      rep(4, length(data_total %>% filter(tournament %in% Tournament_id) %>% pull(year) %>% unique()))))

fig.CAm <- data_total %>% filter(tournament %in% Tournament_id) %>%
  ggplot(aes(x = year, 
             y = rank)) +
  geom_line(data = lines, aes(group = year),
            color = lighten(col,
                            amount = 0.7),
            size = 8) +
  annotate("rect", 
           xmin = 1970.5, 
           xmax = 2022.5, 
           ymin = c(0.6,1.6, 2.6), 
           ymax = c(1.4, 2.4, 3.4), 
           alpha = 0.4, 
           color = medals, 
           fill = medals) +
  annotate("text", 
           x = 1974, 
           y = 1:4, 
           label = c("Campeón", "Subcampeón", "Tercero", "Cuarto"),
           color = c(darken(medals, 
                            amount = 0.6), 
                     col),
           hjust = 1) +
  geom_flag(aes(country = country_id),
            size = 8) +
  geom_label(data = data_total %>% filter(tournament %in% Tournament_id & is_host==1),
             label = "Anfitrión",
             family = "JetBrains Mono",
             size = 2.5, 
             nudge_y = 0.28,
             color = "black",
             fill = "gray80",
             fontface = "bold") +
  coord_cartesian(expand = FALSE,
                  clip = 'off') +
  scale_y_reverse(limits = c(4.2, 0.6)) +
  scale_x_continuous(breaks = data_total %>% filter(tournament %in% Tournament_id) %>% pull(year) %>% unique()) +
  theme_void(base_family = "JetBrains Mono") +
  scale_color_manual(values = pal) +
  labs(title = "Cuatro primeros puestos de la **Copa América** a lo largo de los años<br>",
       caption = "<br/>**Source:** Wikipedia | **Viz:** @spiousas") +
  theme(legend.position = "none",
        plot.margin = unit(c(.2,.24,.2,.24), "cm"),
        plot.title = element_markdown(color = col,
                                      hjust = 0.5),
        plot.title.position = "panel",
        plot.caption = element_markdown(color = col),
        plot.caption.position = "plot",
        axis.text.x = element_markdown(size = 8, 
                                       vjust = 0,
                                       color = col),
        panel.background = element_rect(fill = lighten(col, amount = .9), 
                                        colour = NA),
        plot.background = element_rect(fill = lighten(col, amount = .9), 
                                colour = NA))
fig.CAm

ggsave("./Copa_America/historico/output/copa_america_historico.png", dpi = 600, width = 37, height = 10, units = "cm")
