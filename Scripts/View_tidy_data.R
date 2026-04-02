# Objetivo : limpiar y ordenar datos
rm(list = ls())

setwd("C:/R/R proy tidy data PINV01-528")

# 1. Importar paquetes ----

if (!require('pacman'))
  install.packages('pacman', repos = "http://cran.us.r-project.org")


pacman::p_load(
  tidyverse,
  rio,
  plotly,
  lubridate,
  dplyr,
  gt,
  janitor,
  tinytex
)


# 3. Importar datos de ocurrencia ----

df = rio::import('data/processed_data/SENEPA_tidy_data_set.csv',
                 encoding = "UTF-8")

occs = df

# Dataset

df_tbl <- as_tibble(df) |>
  mutate(
    DATE_aux = paste(YEAR, 1, 1, sep = "-"),
    DATE_aux = if_else(is.na(DATE), as.Date(DATE_aux), as.Date(DATE))
  )


df_tbl |>
  dplyr::select(
    SPECIES = SPECIES_AI_Correct,
    DISEASE,
    YEAR,
    LAT,
    LONG,
    DATE_aux,
    -c(SPECIES_RAW, Justification, SPECIES)
  ) |>
  gt() |>
  cols_label_with(fn = ~ janitor::make_clean_names(., case = "title"))   |>
  tab_style(style = cell_fill(color = "gray95"),
            locations = cells_body(columns = c(SPECIES, DISEASE))) |>
  tab_style(locations = cells_body(columns = SPECIES),
            style = cell_text(weight = "bold")) |>
  opt_interactive(
    use_search = TRUE,
    use_filters = TRUE,
    use_resizers = TRUE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
    use_text_wrapping = TRUE,
    use_page_size_select = TRUE
  ) |>
  tab_header(title = "SENEPA Tidy Dataset",
             subtitle = paste(
               "From",
               min(df_tbl$YEAR, na.rm = T),
               "to",
               max(df_tbl$YEAR, na.rm = T),
               sep = " "
             )) |>
  opt_align_table_header(align = "left") |>
  tab_options(heading.padding = px(1))
