# Objetivo : Exploratory analysis of records by taxonomy, disease, 
# temporal distribution, and spatial distribution
rm(list = ls())

setwd("C:/R/R proy tidy data PINV01-528")

# 1. Importar paquetes ----

if (!require('pacman'))
  install.packages('pacman', repos = "http://cran.us.r-project.org")


pacman::p_load(
  ENMeval,
  tidyverse,
  rio,
  raster,
  plotly,
  dismo,
  summarytools,
  explore,
  dlookr,
  leaflet,
  lubridate,
  dplyr,
  gt,
  janitor,
  stringr,
  ggridges,
  sf,
  geodata,
  hexbin,
  cowplot,
  showtext,
  stringr,
  forcats,
  tinytex
)




# 3. Importar datos de ocurrencia ----

df = rio::import('data/processed_data/SENEPA_tidy_data_set.csv')

occs = df

#head(occs)

occs_sp <- occs |> dplyr::select(LONG, LAT) |> SpatialPoints()


# Taxonomía ----

# 1) Preparar datos (todas las especies)


# ---- TABLA DE FRECUENCIAS --
spp_df <- as.data.frame(table(df$SPECIES_AI_Correct, df$DISEASE)) %>%
  rename(spp = Var1, Disease = Var2, Freq = Freq) %>%
  mutate(
    spp = str_trim(as.character(spp)),
    spp = na_if(spp, "")
  ) %>%
  drop_na(spp) %>%
  filter(Freq > 0)

# 2) Añadir familia taxonómic

spp_df <- spp_df %>%
  left_join(df %>% select(SPECIES_AI_Correct, FAMILY),
            by = c("spp" = "SPECIES_AI_Correct"))

# 3) Ordenar especies dentro de cada enfermedad 

spp_df_ordered <- spp_df %>%
  group_by(Disease) %>%
  mutate(spp = fct_reorder(spp, Freq, .desc = TRUE)) %>%
  ungroup()

#  4) Paleta de colores por familia taxonómica

families <- unique(spp_df_ordered$FAMILY)
pal <- RColorBrewer::brewer.pal(min(length(families), 8), "Dark2")
family_colors <- setNames(pal, families)

# 5) Función para crear un gráfico interactivo por enfermedad

make_plot <- function(df_sub) {
  d <- unique(df_sub$Disease)
  
  p <- ggplot(df_sub,
              aes(x = spp, y = Freq,
                  color = FAMILY,
                  text = paste0(
                    "<b>specie:</b> ", spp,
                    "<br><b>Freq:</b> ", Freq,
                    "<br><b>Family:</b> ", FAMILY,
                    "<br><b>Disease:</b> ", d
                  ))) +
    geom_segment(aes(xend = spp, yend = 0), color = "gray70") +
    geom_point(size = 4) +
    scale_color_manual(values = family_colors) +
    coord_flip() +
    labs(title = d, x = "", y = "") +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  ggplotly(p, tooltip = "text")
}

# 6) Crear lista de gráficos por enfermedad

diseases <- unique(spp_df_ordered$Disease)

plots_list <- lapply(diseases, function(d) {
  df_sub <- spp_df_ordered %>% filter(Disease == d)
  make_plot(df_sub)
})

#  7) Dropdown interactivo para elegir enfermedad

# Esto crea un solo panel donde el usuario selecciona la enfermedad.

fig_OO <- plot_ly()

for (i in seq_along(diseases)) {
  # extraer la capa de puntos del ggplotly
  pts <- plots_list[[i]]$x$data[[2]]
  
  fig_OO <- fig_OO %>% add_trace(
    x = pts$x,
    y = pts$y,
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, color = pts$marker$color),
    text = pts$text,
    hoverinfo = "text",
    visible = ifelse(i == 1, TRUE, FALSE),
    name = diseases[i]
  )
}

# botones
buttons <- lapply(seq_along(diseases), function(i) {
  list(
    method = "update",
    args = list(
      list(visible = seq_along(diseases) == i)
    ),
    label = diseases[i]
  )
})

fig_OO <- fig_OO %>%
  layout(
    updatemenus = list(
      list(
        type = "dropdown",
        x = 1.05, y = 1,
        buttons = buttons
      )
    ),
    title = "Species by Disease (N)"
  )

fig_OO

# Tiempo ----

# 1) Preparar datos (conteos anuales por enfermedad)


d_order <- c(
  "MALARIA",
  "LEISHMANIOSIS",
  "FIEBRE AMARILLA URBANA",
  "FIEBRE AMARILLA",
  "DENGUE",
  "CHAGAS"
)

df_plot <- df %>%
  mutate(
    YEAR_clean = str_extract(as.character(YEAR), "\\d{4}"),
    DATE_aux = if_else(is.na(DATE),
                       as.Date(paste0(YEAR_clean, "-01-01")),
                       as.Date(DATE)),
    DATE2 = as.Date(DATE_aux),
    YEAR = year(DATE2)
  ) %>%
  filter(DISEASE %in% d_order) %>%
  mutate(DISEASE = factor(DISEASE, levels = d_order))

# 2) Conteos por año × enfermedad + acumulados

df_year <- df_plot %>%
  count(DISEASE, YEAR) %>%
  group_by(DISEASE) %>%
  arrange(YEAR) %>%
  mutate(cum = cumsum(n)) %>%
  ungroup()

# 3) Crear figura con dos ejes 

# - Eje izquierdo: conteos anuales
# - Eje derecho: acumulados

fig_T <- plot_ly()

# 4) Añadir trazas por enfermedad (conteos + acumulados)
# Cada enfermedad tendrá dos trazas:
#  - Conteos → eje Y1
# - Acumulados → eje Y2
# Solo la primera enfermedad será visible al inicio.

for(i in seq_along(d_order)) {
  dname <- d_order[i]
  df_sub <- df_year %>% filter(DISEASE == dname)
  
  # Conteos anuales (eje izquierdo)
  fig_T <- fig_T %>% add_trace(
    data = df_sub,
    x = ~YEAR,
    y = ~n,
    type = "scatter",
    mode = "lines+markers",
    name = paste0(dname, " - Conteos"),
    visible = ifelse(i == 1, TRUE, FALSE),
    yaxis = "y1",
    line = list(color = "#1f77b4"),
    text = ~paste(
      "Disease:", DISEASE,
      "<br>Year:", YEAR,
      "<br>Count:", n
    ),
    hoverinfo = "text"
  )
  
  # Acumulados (eje derecho)
  fig_T <- fig_T %>% add_trace(
    data = df_sub,
    x = ~YEAR,
    y = ~cum,
    type = "scatter",
    mode = "lines+markers",
    name = paste0(dname, " - Acumulado"),
    visible = ifelse(i == 1, TRUE, FALSE),
    yaxis = "y2",
    line = list(color = "#d62728", dash = "dash"),
    text = ~paste(
      "Disease:", DISEASE,
      "<br>Year:", YEAR,
      "<br>Cumulative:", cum
    ),
    hoverinfo = "text"
  )
}

# 5) Dropdown para seleccionar enfermedad

# Cada enfermedad controla dos trazas (conteos + acumulados).

buttons <- lapply(seq_along(d_order), function(i) {
  vis <- rep(FALSE, 2 * length(d_order))
  vis[(2*i - 1):(2*i)] <- TRUE
  
  list(
    method = "update",
    args = list(list(visible = vis)),
    label = d_order[i]
  )
})

# ) Layout con doble eje Y + range slider

fig_T <- fig_T %>%
  layout(
    title = "Counts and cumulative totals by disease",
    updatemenus = list(
      list(
        type = "dropdown",
        x = 1.05, y = 1,
        buttons = buttons,
        showactive = TRUE
      )
    ),
    xaxis = list(
      title = "Año",
      rangeslider = list(visible = TRUE)
    ),
    yaxis = list(
      title = "Annual records",
      side = "left"
    ),
    yaxis2 = list(
      title = "Accumulated records",
      overlaying = "y",
      side = "right"
    )
  )

fig_T


# Generar mapa interactivo ----


occs |> dplyr::rename(lng = LONG, lat = LAT) %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -58,
          lat = -24,
          zoom = 5.6) %>%
  addCircleMarkers(
    ~ lng,
    ~ lat,
    radius = 1,
    popup = ~ paste0(
      occs$DISEASE,
      "<hr>",
      occs$SPECIE,
      "<br>",
      occs$LOCALITY,
      "<br>",
      occs$YEAR
    )
  )

# 4. Visualizar datos de ocurrencia en todo el conjunto de datos----

# Make a SpatialPoints object

#summary(occs)

fig <- occs %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  )  %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig

# 4.1 Visualizar datos de ocurrencia FIEBRE AMARILLA URBANA----

#unique(occs$DISEASE)

fig_FAU <- occs %>% subset(DISEASE == "FIEBRE AMARILLA URBANA")
fig_FAU <- fig_FAU %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  )  %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_FAU

# 4.2 Visualizar datos de ocurrencia MALARIA----

#unique(occs$DISEASE)

fig_M = occs %>% subset(DISEASE == "MALARIA") %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  )  %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_M

# 4.3 Visualizar datos de ocurrencia LEISHMANIOSIS----

#unique(occs$DISEASE)

fig_L = occs %>% subset(DISEASE == "LEISHMANIOSIS") %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_L

# 4.4 Visualizar datos de ocurrencia LEISHMANIOSIS----

#unique(occs$DISEASE)

fig_L = occs %>% subset(DISEASE == "LEISHMANIOSIS") %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_L

# 4.6 Visualizar datos de ocurrencia CHAGAS----

unique(occs$DISEASE)

fig_CH = occs %>% subset(DISEASE == "CHAGAS") %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_CH


# 4.7 Visualizar datos de ocurrencia DENGUE----

#unique(occs$DISEASE)

fig_D = occs %>% subset(DISEASE == "DENGUE") %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_D

# 4.8 Visualizar datos de ocurrencia FIEBRE AMARILLA----

unique(occs$DISEASE)

fig_FA = occs %>% subset(DISEASE == "FIEBRE AMARILLA") %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4.6,
      center = list(lon = -58 , lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  )

fig_FA


# 5. Visualización espaciotemporal todos los registros----

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:
occs2 <- occs %>%
  mutate(
    DATE_aux = paste(YEAR, 1, 1, sep = "-"),
    DATE_aux = if_else(is.na(DATE), as.Date(DATE_aux), as.Date(DATE))
  ) %>%
  mutate(DATE2 = as.Date(DATE_aux),
         YEAR = ifelse(is.na(DATE2), NA_character_, format(floor_date(DATE2, "year"), "%Y")))

fig <- occs2 %>%
  filter(!is.na(YEAR)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ YEAR,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", YEAR)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4,
      center = list(lon = -58, lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: "))

fig


# 5.1 Visualización espaciotemporal todos los registros FIEBRE AMARILLA URBANA----

unique(occs$DISEASE)

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:
occs2 <- occs %>%
  mutate(
    DATE_aux = paste(YEAR, 1, 1, sep = "-"),
    DATE_aux = if_else(is.na(DATE), as.Date(DATE_aux), as.Date(DATE))
  ) %>%
  mutate(DATE2 = as.Date(DATE_aux),
         YEAR = ifelse(is.na(DATE2), NA_character_, format(floor_date(DATE2, "year"), "%Y")))

fig_FAU <- occs2 %>%
  filter(DISEASE == "FIEBRE AMARILLA URBANA" & !is.na(YEAR)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ YEAR,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", YEAR)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4,
      center = list(lon = -58, lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: "))

fig_FAU


# 5.2 Visualización espaciotemporal todos los registros MALARIA----

unique(occs$DISEASE)

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:


fig_M <- occs2 %>%
  filter(DISEASE == "MALARIA" & !is.na(YEAR)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ YEAR,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", YEAR)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4,
      center = list(lon = -58, lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: "))

fig_M

# 5.3 Visualización espaciotemporal todos los registros "LEISHMANIOSIS"----

unique(occs$DISEASE)

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:

fig_L <- occs2 %>%
  filter(DISEASE == "LEISHMANIOSIS" & !is.na(YEAR)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ YEAR,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", YEAR)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4,
      center = list(lon = -58, lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: "))

fig_L

# 5.4 Visualización espaciotemporal todos los registros "CHAGAS"----

unique(occs$DISEASE)

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:

fig_CH <- occs2 %>%
  filter(DISEASE == "CHAGAS" & !is.na(YEAR)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ YEAR,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", YEAR)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4,
      center = list(lon = -58, lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: "))

fig_CH


# 5.5 Visualización espaciotemporal todos los registros DENGUE----

#unique(occs$DISEASE)

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:

occs2 <- occs %>%
  mutate(
    DATE_aux = paste(YEAR, 1, 1, sep = "-"),
    DATE_aux = if_else(is.na(DATE), as.Date(DATE_aux), as.Date(DATE))
  ) %>%
  mutate(DATE2 = as.Date(DATE_aux),
         MONTH = ifelse(is.na(DATE2), NA_character_, format(floor_date(DATE2, "month"), "%Y-%m"))) #month

fig_D <- occs2 %>%
  filter(DISEASE == "DENGUE" & !is.na(MONTH)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ MONTH,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", MONTH)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 13.2,
      center = list(lon = -57.65, lat = -25.32)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "MONTH: "))

fig_D

# 5.4 Visualización espaciotemporal todos los registros "FIEBRE AMARILLA"----

unique(occs$DISEASE)

# Si DATE ya fue convertido previamente a Date, no hace falta volver a as.Date()
# Llamada típica:

fig_FA <- occs2 %>%
  filter(DISEASE == "FIEBRE AMARILLA" & !is.na(YEAR)) %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~ LAT,
    lon = ~ LONG,
    coloraxis = 'coloraxis',
    radius = 6,
    frame = ~ YEAR,
    hoverinfo = "text",
    text = ~ paste(SPECIES_CLEAN, "<br>", YEAR)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 4,
      center = list(lon = -58, lat = -23.5)
    ),
    coloraxis = list(colorscale = "Viridis")
  ) %>%
  animation_opts(frame = 1000,
                 transition = 0,
                 redraw = T) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: "))

fig_FA



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




