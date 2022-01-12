pacman::p_load("tidyverse", "ggtext", "here", "lubridate", "pracma", "colorspace", "patchwork")

# Data loading and wrangling
owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
covid <- read_csv(owid_url)
country <- "Argentina"
 <- covid %>% 
  filter(location == country) %>% 
  select(date, new_cases, new_cases_smoothed, new_deaths, new_deaths_smoothed) %>% 
  arrange(date) %>% 
  complete(date = seq(min(.$date), max(.$date), by = 1),
           fill = list(new_cases = 0, new_cases_smoothed = 0, new_deaths = 0, new_deaths_smoothed = 0)) %>% 
  mutate(day_of_year = yday(date),
         year = year(date)
  )

# Scales
size_factor_cases <- 500
size_factor_deaths <- 50000

# Colors
cases_color <- "#588300"
deaths_color <- "#990000"
base_grey <- "grey28"

# Month definition
month_length <- c(31, 28, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- cumsum(month_length) - 30

# Text definitions
text_color <- rgb(18, 18, 18, maxColorValue = 255)
base_family <- "Helvetica"
subtitle_date <- max(covid_cases$date) %>% 
  format("%b. %d, %Y")

# Annotations for the years in a list (used in annotate())
year_annotations <- list(
  year = 2020:2022,
  x = rep(3, 3),
  y = as.POSIXct(paste(2020:2022, "01", "01", sep = "-"))
)

p <- covid_cases %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(date != as_date("2020-02-29")) %>%
  group_by(year) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() %>%
  ggplot() +
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date),
                  ymax = as.POSIXct(date) + new_cases_smoothed / 2 * size_factor_cases,
                  group = year),
              size = 0.3, col = cases_color, fill = lighten(cases_color, .6), show.legend = FALSE) +
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date) - new_deaths_smoothed / 2 * size_factor_deaths,
                  ymax = as.POSIXct(date),
                  group = year),
              size = 0.3, col = deaths_color, fill = lighten(deaths_color, .6), show.legend = FALSE) +
  # Basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date) + days(1)),
               col = base_grey, size = 0.5) +
  # Annotation: Promedio
  annotate("richtext",
           label = "Promedio<br>de 7 días",
           x = 200, y = as.POSIXct("2021-12-01"),
           family = base_family, size =3, color = text_color,
           label.colour = NA, fill = NA, vjust = 1) +
  annotate("segment",
           x = 200, xend = 200,
           y = as.POSIXct("2021-12-01"), yend = as.POSIXct("2021-08-01"),
           color = text_color, size = 0.3) +
  # Annotation: years
  annotate("text", label = paste0(year_annotations$year, "\u2192"), x = year_annotations$x, 
           y = year_annotations$y, 
           family = "Arial",
           size = 2.5, vjust = -1.3, hjust = 0.15) +   
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks[c(1, 4, 7, 10)],
                     labels = c("Enero", "Abril", "Julio", "Octubre"),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  labs(caption = "**Fuente:** Our World in Data | **Viz:** @spiousas<br/>") +
  #coord_polar() +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    plot.caption = element_markdown(color = "black", size = 6, hjust = 0.5),
    axis.text.x = element_text(color = base_grey, size = 8, hjust = 0.5)
  )

# Legend
p_legend <- 
  tibble(
    xaxis = c(0, 200000),
    cases = c(0, 50000),
    deaths = c(0, -500),
  ) %>% 
  ggplot(aes(xaxis)) +
  geom_ribbon(aes(ymin = 0, ymax = size_factor_cases * cases),
              col = cases_color, fill = lighten(cases_color, .6), size = 0.3) +
  geom_ribbon(aes(ymin =  size_factor_deaths *deaths, ymax = 0),
              col = deaths_color, fill = lighten(deaths_color, .6), size = 0.3) +
  geom_line(aes(y = 1), color = base_grey) +
  geom_text(label = 0, y = 0, x = -10000, size = 2.5, hjust = 1, vjust = .5) +
  geom_text(label = "100k casos", y = size_factor_cases * 50000/2, x = 210000, size = 2.5, hjust = 0, vjust = .5) +
  geom_text(label = "1k muertes", y = size_factor_deaths * (-500)/2, x = 210000, size = 2.5, hjust = 0, vjust = .5) +
  coord_cartesian(xlim = c(-.5e5, 3e5), 
                  ylim = c(-as.numeric(as.POSIXct("1971-01-01")), NA), 
                  clip = "off") + 
  labs(title = "Casos y muertes por<br>Covid-19, Argentina") +
  theme_void() +
  theme(plot.title = element_markdown(color = text_color, 
                                      family = "Helvetica",
                                      face = "bold", size = 8, hjust = 0.5,
                                      lineheight = 1.1))

# Add plot and legend
p + inset_element(p_legend, left = 0.05, bottom = 0.725, right = 0.25, top = 0.95)

# Save
ggsave(here("spiral-19", "output", "spiral_arg.png"), width = 15, height = 15 , units = "cm")

