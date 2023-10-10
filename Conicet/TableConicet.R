pacman::p_load(tidyverse, ggalluvial, janitor, ggtext, extrafont, here, gt, 
               gtExtras, readxl, scales)

data_2018 <- read_delim(here("Conicet/data/Personal_por_categoria.csv"), delim = ";") %>%
  clean_names() %>%
  count(genero, categoria, name = "n_2018") %>%
  mutate(categoria = factor(categoria, levels = c("Inv. Superior", 
                                                  "Inv. Principal", 
                                                  "Inv. Independiente", 
                                                  "Inv. Adjunto", 
                                                  "Inv. Asistente")),
         genero = factor(genero, levels = c("Mujeres", 
                                            "Hombres"))) 


data_2022 <- read_xlsx(here("Conicet/data/data_coniet.xlsx")) %>%
  clean_names() %>%
  count(genero, categoria, name = "n_2022") %>%
  filter(genero != "Otro") %>%
  mutate(categoria = paste0("Inv. ", str_to_title(categoria)),
         categoria = factor(categoria, levels = c("Inv. Superior", 
                                                  "Inv. Principal", 
                                                  "Inv. Independiente", 
                                                  "Inv. Adjunto", 
                                                  "Inv. Asistente")),
         genero = factor(genero, levels = c("Mujeres", 
                                            "Hombres"))) 

data_completa <- left_join(data_2018, data_2022) 

tab_conicet <- data_completa %>% 
  group_by(categoria) %>% 
  summarise(N_2018 = sum(n_2018),
            N_2022 = sum(n_2022),
            ratio_2018 = n_2018[genero == "Mujeres"]/n_2018[genero == "Hombres"],
            ratio_2022 = n_2022[genero == "Mujeres"]/n_2022[genero == "Hombres"],
            dif_ratio = (ratio_2022 - ratio_2018)*100/ratio_2018) %>%
  gt(rowname_col = "categoria") %>%
  cols_label(
    categoria = 'Categoría',
    N_2018 = 'Total',
    N_2022 = 'Total',
    ratio_2018 = 'M/V',
    ratio_2022 = 'M/V',
    dif_ratio = 'Cambio %'
  ) %>%
  tab_spanner(
    label = md('**2018**'),
    columns = c('N_2018', 'ratio_2018')
  ) %>%
  tab_spanner(
    label = md('**2022**'),
    columns = c('N_2022', 'ratio_2022')
  ) %>%
  tab_spanner(
    label = md('**2018 a 22**'),
    columns = c('dif_ratio')
  ) %>% 
  tab_header(
    title = 'Género y categoría de lxs investigadorxs de Conicet',
    subtitle = md('La siguiente tabla es una comparativa entre la cantidad de Varones y Mujeres en 
    cada categoría de la **Carrera de Investigador/a Científico/a** del organismo. Puede verse que, 
    si bien hay más investigadoras mujeres que varones, esto no es así para la **categoría más alta**,
    en la que hay aproximadamente **una mujer cada tres varones**. Por otro lado, en la **categoría más baja**
    hay aproximadamente d**os varones cada tres mujeres** y la categoría de **Inv. Independiente** sigue siendo
    el pivote de la tijera. El dato alentador es que el *ratio* Mujeres/Varones (M/V)
    mejoró principalmente en las categorías más altas (**Inv. Principal** e **Inv. Superior**).')) %>%
  cols_align(align = 'center', 
             columns = where(is.numeric))  %>%
  cols_width(c(dif_ratio, N_2018, N_2022, ratio_2018, ratio_2022) ~ px(90),
             categoria ~ px(150)) %>%
  cols_align(
    align = 'left', 
    columns = where(is.factor)
  ) %>%
  fmt_number(
    columns = c("ratio_2018", "ratio_2022", "dif_ratio"),
    decimals = 2,
    use_seps = FALSE
  ) %>%
  tab_footnote(
    footnote = "M/V expreza la razón de cantidad de investigadoras mujeres e investigadores varones.",
    locations = cells_column_labels(columns = c("ratio_2018", "ratio_2022"))
  ) %>%
  gt_theme_538() %>%
  gt_color_rows(c(ratio_2018, ratio_2022), pal_type = "discrete", direction = -1,
                palette = "PRGn", domain = c(0.2, 1.8)) %>%
  gt_color_rows(dif_ratio, pal_type = "discrete", direction = -1,
                palette = "RdYlBu", domain = c(-8, 8)) %>%
  grand_summary_rows(
    columns = c(N_2018, N_2022),
    fns =  list(label = md("**TOTAL**"), id = "totals", fn = "sum"),
  ) %>%
  tab_style(
    locations = cells_grand_summary(),
    style = cell_text(weight = "bold")
  ) %>%
  tab_source_note(md("**Tabla**: @spiousas | **Datos**: Conicet"))
  
tab_conicet

tab_conicet %>% gtsave(here("Conicet/output/tabla_genero.png"), 
                       expand = 30)

