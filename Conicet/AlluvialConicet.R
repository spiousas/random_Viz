pacman::p_load(tidyverse, ggalluvial, janitor, ggtext, extrafont, here)

read_delim(here("Conicet/data/Personal_por_categoria.csv"), delim = ";") %>%
  clean_names() %>%
  group_by(genero, categoria) %>%
  summarise(n = n()) %>%
  mutate(categoria = factor(categoria, levels = c("Inv. Superior", 
                                                  "Inv. Principal", 
                                                  "Inv. Independiente", 
                                                  "Inv. Adjunto", 
                                                  "Inv. Asistente")),
         genero = factor(genero, levels = c("Mujeres", 
                                            "Hombres"))) %>%
  ggplot(aes(axis1 = genero, axis2 = categoria, y = n)) +
    scale_x_discrete(limits = c("genero", "categoria"), expand = c(.2, .05)) +
    geom_alluvium(aes(fill = genero), width = 1/12) +
    geom_stratum(fill = "black", 
                 alpha = .5,
                 color = "black",
                 size = .7,
                 width = 1/12) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              color = "white",
              family = "JetBrains Mono",
              angle = c(90, 90, 0, 0, 0, 0, 0),
              hjust = c(0.5, 0.5, 0, 0, 0, 0, 0),
              vjust = 0.5,
              nudge_x = c(0, 0, 0.05, 0.05, 0.05, 0.05, 0.05),
              size = c(4, 4, 3, 3, 3, 3, 3))+
    theme_void(base_family = "JetBrains Mono") +
    labs(title = "Género y categoría de lxs investigadorxs de **Conicet**",
         subtitle = "<br>La composición de la planta de investigadorxs de Conicet cuenta con más mujeres (**5905**) que hombres (**5102**).<br>
         Sin embargo, este desbalanceo no se replica en todas las catergorías, estando sobrerepresentados los hombres en <br>
         las categorías de Inv. Independiente (**1385** hombres vs. **1352** mujeres), Principal (**707** hombres vs. **511** mujeres) y<br>Superior (**161** hombres vs. **54** mujeres).",
         caption = "**Fuente:** Base de Datos del CONICET | **Viz:** @spiousas<br/>") +
    scale_fill_brewer(palette = "Set2", 
                      direction = -1) +
    theme(panel.background = element_rect(fill = "gray30", color = NA),
          plot.background = element_rect(fill = "gray30", color = NA),
          plot.title = element_markdown(color = "white", margin = unit(c(0,0,0,2.5), "cm")),
          plot.subtitle = element_markdown(color = "white", margin = unit(c(0,0,0,2.5), "cm")),
          plot.caption = element_markdown(color = "white", margin = unit(c(0,0,0,2), "cm")),
          legend.position = "none",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.margin = unit(c(0.2,0.2,0.2,-2), "cm")
          )

ggsave("./Conicet/output//investConicet.png", dpi = 300, width = 27, height = 20, units = "cm")