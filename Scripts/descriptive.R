# Objetivo : limpiar y ordenar datos
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
  tinytex
)


# 3. Importar datos de ocurrencia ----

df = rio::import('data/processed_data/SENEPA_tidy_data_set.csv',
                 encoding = "UTF-8")

occs = as_tibble(df)

head(occs)

occs_sp <- occs |> dplyr::select(LONG, LAT) |> SpatialPoints()


# ORGANISMO ----
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)

spp_df <- as.data.frame(table(df$SPECIES_AI_Correct, df$DISEASE)) %>%
  rename(Var1 = Var1, Var2 = Var2, Freq = Freq) %>%
  mutate(
    Var1 = as.character(Var1),
    Var1 = str_trim(Var1),
    Var1 = na_if(Var1, "")   # "" -> NA
  ) %>%
  drop_na(Var1) %>%
  filter(Freq > 0) %>%
  rename(spp = Var1, Disease = Var2)

# Order species within each Disease by descending frequency
spp_df_ordered <- spp_df %>%
  group_by(Disease) %>%
  mutate(spp = fct_reorder(spp, Freq, .desc = F)) %>%
  ungroup()

# If you want the same ordering to be preserved across facets (so each facet has its own order),
# keep spp as factor per group as above. Now plot:
plot_oo = ggplot(spp_df_ordered, aes(x = spp, y = Freq, fill = Disease)) +
  geom_segment(aes(xend = spp, yend = 0), color = "gray70") +
  geom_point(aes(color = Disease), size = 1) +
  facet_wrap( ~ Disease, ncol = 1, scales = "free") +
  coord_flip() +
  theme_classic() +
  xlab("") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )

plot_oo

# Guardar
ggsave(
  "output/fig/disease_spp.svg",
  plot_oo,
  width = 6,
  height = 22,
  dpi = 500
)

# Guardar
ggsave(
  "output/fig/disease_spp.pdf",
  plot_oo,
  width = 6,
  height = 22,
  dpi = 500
)


# TIEMPO ----


# vector con el orden deseado
d_order <- c(
  "MALARIA",
  "LEISHMANIOSIS",
  "FIEBRE AMARILLA URBANA",
  "FIEBRE AMARILLA",
  "DENGUE",
  "CHAGAS"
)

# preparar datos y forzar niveles (mantener otras enfermedades al final si existen)
df_plot <- df %>%
  mutate(
    YEAR_clean = str_extract(as.character(YEAR), "\\d{4}"),
    DATE_aux = if_else(is.na(DATE), as.Date(paste0(
      YEAR_clean, "-01-01"
    )), as.Date(DATE)),
    DATE2 = as.Date(DATE_aux),
    MONTH = if_else(!is.na(DATE2), format(floor_date(DATE2, "month"), "%Y-%m"), NA_character_)
  ) %>%
  # opcional: filtrar solo las enfermedades de interés
  filter(DISEASE %in% d_order) %>%
  mutate(DISEASE = factor(DISEASE, levels = d_order)) %>%
  filter(!is.na(DATE2) & !is.na(DISEASE))

# calcular N por DISEASE y posición para etiqueta
counts_pos <- df_plot %>%
  count(DISEASE) %>%
  mutate(label = paste0("N=", n))

# posición x para etiquetas (un poco más allá del máximo DATE2)
x_label_pos <- max(df_plot$DATE2, na.rm = TRUE) - 100   # ajustar margen en días si hace falta

# plot con niveles forzados
plot_t <- ggplot(df_plot, aes(x = DATE2, y = DISEASE, fill = DISEASE)) +
  geom_density_ridges(alpha = 0.6, stat = "binline", bins = 20) +
  geom_text(
    data = counts_pos,
    aes(x = x_label_pos, y = DISEASE, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    size = 3.5
  ) +
  scale_y_discrete(limits = d_order) +   # refuerza el orden en el eje y
  scale_x_date(date_labels = "%Y", expand = expansion(mult = c(0.01, 0.12))) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  xlab("") + ylab("")

plot_t

# Guardar
ggsave(
  "output/fig/disease_time.svg",
  plot_t,
  width = 5,
  height = 14,
  dpi = 500
)

# Guardar
ggsave(
  "output/fig/disease_time.pdf",
  plot_t,
  width = 5,
  height = 14,
  dpi = 500
)





# LUGAR ----

# Opcional: fuentes (como en el ejemplo)
# font_add_google("DM Serif Display", "abril")
# font_add_google("Tajawal", "tawa")
# showtext_auto()

# 1) Obtener límites administrativos de Paraguay (level = 1 -> departamentos)
#    Si no querés instalar geodata, puedes usar rnaturalearth::ne_states()
pry_adm1 <- geodata::gadm(country = "PRY",
                          level = 1,
                          path = tempdir())
# gadm devuelve un sf; renombrar si hace falta
pry_adm1 <- st_as_sf(pry_adm1)

# 2) Preparar datos de puntos
#    Ajusta nombres de columnas si son diferentes.
df2 <- df %>%
  mutate(
    # fallback: si DATE es NA, construir desde YEAR (primer día del año)
    YEAR_chr = str_extract(as.character(YEAR), "\\d{4}"),
    DATE_aux = if_else(is.na(DATE), as.Date(paste0(YEAR_chr, "-01-01")), as.Date(DATE)),
    DATE2 = as.Date(DATE_aux)
  ) %>%
  filter(!is.na(LAT) & !is.na(LONG)) %>%
  mutate(LONG = as.numeric(LONG), LAT = as.numeric(LAT))

# 3) Lista de enfermedades a mapear (puedes filtrar o ordenar)
diseases <- df2 %>% distinct(DISEASE) %>%
  subset(DISEASE != "DENGUE") %>%
  arrange(DISEASE) %>% pull(DISEASE)

# 4) Función que crea un mapa para una enfermedad concreta
make_map_for_disease <- function(disease_name,
                                 points_df,
                                 adm_sf,
                                 bins = 5,
                                 palette_low = "#D1F0E5",
                                 palette_high = "#306D75") {
  sub <- points_df %>% filter(DISEASE == disease_name)
  n_obs <- nrow(sub)
  # si no hay observaciones, devolver un plot vacío con mensaje
  if (n_obs == 0) {
    p_empty <- ggplot() +
      geom_sf(
        data = adm_sf,
        fill = "transparent",
        color = "darkgray",
        size = 0.25
      ) +
      ggtitle(paste0(disease_name, " (N=0)")) +
      theme_classic()
    return(p_empty)
  }
  p <- ggplot() +
    # hex density de puntos
    geom_hex(
      data = sub,
      aes(x = LONG, y = LAT, fill = ..count..),
      color = "white",
      bins = bins,
      alpha = 1
    ) +
    # límites administrativos
    geom_sf(
      data = adm_sf,
      fill = "transparent",
      color = "darkgray",
      size = 0.25
    ) +
    scale_fill_gradient(low = palette_low, high = palette_high, name = "Count") +
    labs(title = disease_name, subtitle = paste0("N = ", n_obs)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      #legend.position = "bottom",
      legend.key.width = unit(2, "mm")
    )
  return(p)
}

# 5) Crear lista de plots (uno por DISEASE)
plots_list <- lapply(diseases, function(d) {
  # personalizar paleta por tipo si querés (ejemplo simple)
  pal <- switch(
    d,
    "BlueZone" = c("#D1F0E5", "#306D75"),
    "GreenZone" = c("#E1FCC1", "#5DAA01"),
    "DUM" = c("#FFD2D2", "#CD0000"),
    c("#DEA5F0", "#2A0736")
  )
  make_map_for_disease(
    d,
    df2,
    pry_adm1,
    bins = 15,
    palette_low = pal[1],
    palette_high = pal[2]
  )
})

# 6) Combinar plots en una sola fila/tabla (ajusta nrow/ncol según cantidad)
#    Aquí: 3 por fila (como en el ejemplo). Si hay más, se ajusta automáticamente.
ncol_facets <- 1
grid_plots <- plot_grid(plotlist = plots_list, ncol = ncol_facets)

# 7) Header y caption (estilo del PDF)
header_plot <- ggdraw() + draw_label(
  # "Distribución por Enfermedad - Departamentos Paraguay",
  " ",
  fontface = "bold",
  size = 15
)
caption_plot <- ggdraw() + draw_label(
  #"Fuente: tus datos | Visualización: tu nombre",
  "",
  size = 10,
  hjust = 0.5
)

plot_spatial <- plot_grid(
  header_plot,
  grid_plots,
  caption_plot,
  ncol = 1,
  rel_heights = c(0.08, 1, 0.005)
)

plot_spatial


# Guardar
ggsave(
  "output/fig/disease_spatial.svg",
  plot_spatial,
  width = 5,
  height = 14,
  dpi = 500
)


# Quien cuando y donde ----

# plot_grid(p1, plot_t, rel_widths = c(2, 1))

final = plot_grid(
  plot_oo,
  plot_t,
  plot_spatial,
  ncol = 3,
  labels = c("A", "B", "C"),
  rel_widths = c(2, 1, 1),
  rel_heights = c(1, 1, 1)
)

final

# 8) Guardar
#ggsave(
#  "maps_by_disease_paraguay.png",
#  final,
#  width = 4,
#  height = 6 ,
#  dpi = 500
#)


# summario ----

summarytools::view(
  dfSummary(df),
  footnote = NA,
  valid.col = FALSE,
  file = paste("./", "summary.html", sep = "")
)


# Diccionario de datos ----

df  %>%  data_dict_md(output_dir = "./")
