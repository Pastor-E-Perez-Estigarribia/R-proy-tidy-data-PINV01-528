# Cargar librerías necesarias

# Objetivo : limpiar y ordenar datos
rm(list = ls())

setwd("C:/R/R proy tidy data PINV01-528")

# 0. Instalar y cargar paquetes----

require('pacman')

# if (!require('pacman'))
#   install.packages("pacman")
# library(pacman)


pacman::p_load(rio,
               tidyverse,
               summarytools,
               readxl,
               writexl,
               stringr,
               sf,
               sp,
               terra,
               openxlsx,
               dplyr, 
               taxize,
               tinytex)

#tinytex::install_tinytex()   # instala una distribución LaTeX mínima usable por R

# Initialize processing log ----
processing_log <- tibble::tibble(
  timestamp      = character(),
  step           = character(),
  description    = character(),
  rows_affected  = numeric(),
  variables      = character(),
  operator       = "Pastor-Diego-Oscar"
)

# Function to append entries to the log ----
add_log <- function(step,
                    description,
                    rows_affected = NA,
                    variables = NA) {
  new_entry <- tibble::tibble(
    timestamp     = as.character(Sys.time()),
    step          = step,
    description   = description,
    rows_affected = rows_affected,
    variables     = paste(variables, collapse = ", "),
    operator      = "Pastor-Diego-Oscar"
  )
  
  assign("processing_log",
         dplyr::bind_rows(processing_log, new_entry),
         envir = .GlobalEnv)
}

# Leer los archivo en Excel
# ____________________________________________________________
# Load raw files ----
# ____________________________________________________________

df_0 <- rio::import(
  "./data/raw_data/Base_Flebotomos_ProcesadaV2.xlsx",
  sheet = "Hoja1",
  encoding = "UTF-8"
)
add_log("import",
        "Imported Base_Flebotomos_ProcesadaV2.xlsx",
        nrow(df_0),
        names(df_0))

df_1 <- rio::import(
  "./data/raw_data/base_triatominos2008_2023_copia.xlsx",
  sheet = "Dataset_completo",
  encoding = "UTF-8"
)
add_log("import",
        "Imported base_triatominos2008_2023_copia.xlsx",
        nrow(df_1),
        names(df_1))

df_2 <- rio::import("data/raw_data/full_df_Topa.csv", encoding = "UTF-8")
add_log("import", "Imported full_df_Topa.csv", nrow(df_2), names(df_2))

df_3 <- rio::import(
  "./data/raw_data/base_aedes_albopictus.xlsx",
  sheet = "Base",
  encoding = "UTF-8"
)
add_log("import",
        "Imported base_aedes_albopictus.xlsx",
        nrow(df_3),
        names(df_3))

df_4 <- rio::import("data/raw_data/dataset_malariaPIV01.csv", encoding = "UTF-8")
add_log("import",
        "Imported dataset_malariaPIV01.csv",
        nrow(df_4),
        names(df_4))

df_5 <- rio::import("./data/raw_data/BaseFA_2025.xlsx",
                    sheet = "Hoja1",
                    encoding = "UTF-8")
add_log("import", "Imported BaseFA_2025.xlsx", nrow(df_5), names(df_5))


#2. Previsialización de datos ----


# ____________________________________________________________
# 2. Standardize column names (UPPERCASE + remove non‑alphabetic chars) ----
# ____________________________________________________________

standardize_names <- function(df, df_name) {
  old_names <- names(df)
  
  names(df) <- df %>%
    names() %>%
    str_to_upper() %>%
    str_replace_all("[^[:alpha:]]", "_")
  
  add_log(
    step = "standardize_names",
    description = paste0(
      "Standardized column names for ",
      df_name,
      " (UPPERCASE + removed non-alphabetic characters)"
    ),
    rows_affected = 0,
    variables = names(df)
  )
  
  return(df)
}

df_0 <- standardize_names(df_0, "df_0")
df_1 <- standardize_names(df_1, "df_1")
df_2 <- standardize_names(df_2, "df_2")
df_3 <- standardize_names(df_3, "df_3")
df_4 <- standardize_names(df_4, "df_4")
df_5 <- standardize_names(df_5, "df_5")


# ____________________________________________________________
# 3. Processing of Phlebotomine dataset (df_0) ----
# ____________________________________________________________


# Store initial row count for logging
initial_rows_df0 <- nrow(df_0)

df_0_tidy <- df_0  %>%
  
  # Convert all character fields to UPPERCASE
  dplyr::mutate_all(toupper) %>%
  
  # Replace non-informative strings with NA
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/D"))) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "SIN DATOS"))) %>%
  
  # Normalize encoding to ASCII
  dplyr::mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  
  # Build species name from GENUS + SPECIES
  dplyr::mutate(ESPECIE = paste(GENERO, ESPECIE)) %>%
  
  # Assign disease label
  dplyr::mutate(ENFERMEDAD = "LEISHMANIOSIS") %>%
  
  # Assign UTM metadata
  dplyr::mutate(UTM_ZONA = 21, UTM_HEMISFERIO = "S") %>%
  
  # Rename locality field
  dplyr::rename(LOCALIDAD = LOCALIDAD_BARRIO) %>%
  
  # Select standardized columns
  dplyr::select(
    DEPARTAMENTO,
    DISTRITO,
    LOCALIDAD,
    ANO,
    FECHA,
    ENFERMEDAD,
    ESPECIE,
    UTM_ZONA,
    UTM_HEMISFERIO,
    UTM_X,
    UTM_Y,
    LAT,
    LONG,
    TIPO_DE_GEORREFERENCIACION
  )#%>%

unique(df_0_tidy$ESPECIE)

unique(df_0_tidy$ENFERMEDAD)

# Spatial validation: remove coordinates outside Paraguay bounds
# dplyr::mutate(
#    LAT  = as.numeric(ifelse(LAT  < -28 | LAT  > -19, NA, LAT)),
#    LONG = as.numeric(ifelse(LONG < -63 | LONG > -54, NA, LONG))
#  )

# ____________________________________________________________
# Logging the transformation
# ____________________________________________________________

add_log(
  step = "process_df_0",
  description = paste0(
    "Processed Phlebotomine dataset: uppercase conversion, NA normalization, ",
    "ASCII transliteration, year cleaning, species concatenation, UTM assignment, ",
    "column selection, and spatial validation."
  ),
  rows_affected = nrow(df_0_tidy),
  variables = names(df_0_tidy)
)

# Optional: inspect
#View(df_0_tidy)

# ____________________________________________________________
# 4. Processing of Triatomine dataset (df_1)
# ____________________________________________________________

initial_rows_df1 <- nrow(df_1)

df_1_tidy <- df_1 %>%
  
  # Convert all character fields to UPPERCASE
  dplyr::mutate_all(toupper) %>%
  
  # Replace non-informative strings with NA
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/D"))) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "SIN DATOS"))) %>%
  
  # Normalize encoding to ASCII
  dplyr::mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  
  # Fix inconsistent department names
  dplyr::mutate(DEPARTAMENTO = gsub("CORDILLERA/CAAGUAZU", "CORDILLERA", DEPARTAMENTO)) %>%
  dplyr::mutate(DEPARTAMENTO = gsub("CAPITAL", "ASUNCION", DEPARTAMENTO)) %>%
  
  # Fix inconsistent district/locality labels
  dplyr::mutate(DISTRITO  = gsub(
    "DATOS CONSIGNADOS POR EL OPERADOR DE CAMPO DEL SENEPA",
    "CAMPO",
    DISTRITO
  )) %>%
  dplyr::mutate(LOCALIDAD = gsub(
    "DATOS CONSIGNADOS POR EL OPERADOR DE CAMPO DEL SENEPA",
    "CAMPO",
    LOCALIDAD
  )) %>%
  
  # Clean year field
  dplyr::mutate(ANO = gsub("2007(2012)", 2012, ANO)) %>%
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  dplyr::mutate(ANO = na_if(ANO, 0)) %>%
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  
  # Clean georeferencing field
  dplyr::mutate(
    TIPO_DE_GEORREFERENCIACION =
      gsub(
        "DATOS CONSIGNADOS EN DATOS CONSIGNADOS EN GABINETE",
        "DATOS CONSIGNADOS EN GABINETE",
        TIPO_DE_GEORREFERENCIACION
      )
  ) %>%
  dplyr::mutate(
    TIPO_DE_GEORREFERENCIACION =
      gsub(
        "DATOS CONSIGNADOS POR EL OPERADOR DE DATOS CONSIGNADOS POR EL OPERADOR DE CAMPO DEL SENEPA DEL SENEPA",
        "DATOS CONSIGNADOS POR EL OPERADOR DE CAMPO DEL SENEPA",
        TIPO_DE_GEORREFERENCIACION
      )
  ) %>%
  
  # Standardize species names
  dplyr::mutate(ESPECIE = gsub("GENICULATUS", "P.GENICULATUS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("P. P.GENICULATUS", "P.GENICULATUS", ESPECIE)) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/E"))) %>%
  
  # Assign missing fields
  dplyr::mutate(FECHA = NA) %>%
  dplyr::mutate(ENFERMEDAD = "CHAGAS") %>%
  
  # Expand species names to full binomials
  dplyr::mutate(ESPECIE = gsub("GENICULATUS", "PANSTRONGYLUS GENICULATUS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub(
    "P. PANSTRONGYLUS GENICULATUS",
    "PANSTRONGYLUS GENICULATUS",
    ESPECIE
  )) %>%
  dplyr::mutate(ESPECIE = gsub("SORDIDA", "TRIATOMA SORDIDA", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("INFESTANS", "TRIATOMA INFESTANS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("GUASAYANA", "TRIATOMA GUASAYANA", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("GUASU", "TRIATOMA GUASU", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("P. MEGISTUS", "PANSTRONGYLUS MEGISTUS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("PLATENSIS", "TRIATOMA PLATENSIS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("S/E", "NA", ESPECIE)) %>%
  
  # Assign UTM metadata
  dplyr::mutate(UTM_ZONA = 21, UTM_HEMISFERIO = "S") %>%
  
  # Select standardized columns
  dplyr::select(
    DEPARTAMENTO,
    DISTRITO,
    LOCALIDAD,
    ANO,
    FECHA,
    ENFERMEDAD,
    ESPECIE,
    UTM_ZONA,
    UTM_HEMISFERIO,
    UTM_X,
    UTM_Y,
    LAT,
    LONG,
    TIPO_DE_GEORREFERENCIACION
  ) #%>%

# Spatial validation: remove coordinates outside Paraguay bounds
# dplyr::mutate(
#    LAT  = as.numeric(ifelse(LAT  < -28 | LAT  > -19, NA, LAT)),
#    LONG = as.numeric(ifelse(LONG < -63 | LONG > -54, NA, LONG))
#  )

# ____________________________________________________________
# Logging the transformation
# ____________________________________________________________

add_log(
  step = "process_df_1",
  description = paste0(
    "Processed Triatomine dataset: uppercase conversion, NA normalization, ",
    "ASCII transliteration, corrections to DEPARTAMENTO/DISTRITO/LOCALIDAD, ",
    "year cleaning, georeferencing field standardization, species name harmonization, ",
    "UTM assignment, column selection, and spatial validation."
  ),
  rows_affected = nrow(df_1_tidy),
  variables = names(df_1_tidy)
)

# Optional inspection
#View(df_1_tidy)


# ____________________________________________________________
# 5. Processing of Aedes aegypti dataset (df_2) ----
# ____________________________________________________________

initial_rows_df2 <- nrow(df_2)

df_2_tidy <- df_2 %>%
  
  # Convert all character fields to UPPERCASE
  dplyr::mutate_all(toupper) %>%
  
  # Replace non-informative strings with NA
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/D"))) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "SIN DATOS"))) %>%
  
  # Normalize encoding to ASCII
  dplyr::mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  
  # Rename year field
  dplyr::rename(ANO = VISIT_YEAR) %>%
  
  # Build georeferencing provenance field
  dplyr::mutate(TIPO_DE_GEORREFERENCIACION = paste0(SOURCE_FILE, " ", SUB_DATASET, " ", ZONA_I_PERIOD)) %>%
  
  # Clean year field
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  dplyr::mutate(ANO = na_if(ANO, 0)) %>%
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  
  # Assign administrative units
  dplyr::mutate(DEPARTAMENTO = "CENTRAL") %>%
  dplyr::mutate(DISTRITO = "ASUNCION") %>%
  dplyr::mutate(LOCALIDAD = BARRIO_SENEPA) %>%
  
  # Assign disease and species
  dplyr::mutate(ENFERMEDAD = "DENGUE") %>%
  dplyr::mutate(ESPECIE = "AEDES AEGYPTI") %>%
  
  # Assign UTM metadata
  dplyr::mutate(UTM_ZONA = 21, UTM_HEMISFERIO = "S") %>%
  
  # Assign coordinates and date fields
  dplyr::mutate(
    FECHA = VISIT_DATE,
    UTM_X = Y,
    UTM_Y = X,
    LAT   = Y_,
    LONG  = X_
  ) %>%
  
  # Select standardized columns
  dplyr::select(
    DEPARTAMENTO,
    DISTRITO,
    LOCALIDAD,
    ANO,
    FECHA,
    ENFERMEDAD,
    ESPECIE,
    UTM_ZONA,
    UTM_HEMISFERIO,
    UTM_X,
    UTM_Y,
    LAT,
    LONG,
    TIPO_DE_GEORREFERENCIACION
  ) #%>%

# Spatial validation: remove coordinates outside Paraguay bounds
# dplyr::mutate(
#    LAT  = as.numeric(ifelse(LAT  < -28 | LAT  > -19, NA, LAT)),
#    LONG = as.numeric(ifelse(LONG < -63 | LONG > -54, NA, LONG))
#  )

# ____________________________________________________________
# Logging the transformation
# ____________________________________________________________

add_log(
  step = "process_df_2",
  description = paste0(
    "Processed Aedes aegypti dataset: uppercase conversion, NA normalization, ",
    "ASCII transliteration, year cleaning, administrative assignment, ",
    "species/disease tagging, coordinate mapping, UTM assignment, ",
    "column selection, and spatial validation."
  ),
  rows_affected = nrow(df_2_tidy),
  variables = names(df_2_tidy)
)

# Optional inspection
#View(df_2_tidy)

df_2_tidy$ANO = as.numeric(df_2_tidy$ANO)

# ____________________________________________________________
# 6. Processing of Aedes albopictus dataset (df_3) ----
# ____________________________________________________________

initial_rows_df3 <- nrow(df_3)

df_3_tidy <- df_3 %>%
  
  # Convert all character fields to UPPERCASE
  dplyr::mutate_all(toupper) %>%
  
  # Replace non-informative strings with NA
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/D"))) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "SIN DATOS"))) %>%
  
  # Normalize encoding to ASCII
  dplyr::mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  
  # Clean year field
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  dplyr::mutate(ANO = na_if(ANO, 0)) %>%
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  
  # Assign disease label
  dplyr::mutate(ENFERMEDAD = "FIEBRE AMARILLA URBANA") %>%
  
  # Rename species field
  dplyr::rename(ESPECIE = GENERO_ESPECIE) %>%
  
  # Assign UTM metadata
  dplyr::mutate(UTM_ZONA = 21, UTM_HEMISFERIO = "S") %>%
  
  # Select standardized columns
  dplyr::select(
    DEPARTAMENTO,
    DISTRITO,
    LOCALIDAD,
    ANO,
    FECHA,
    ENFERMEDAD,
    ESPECIE,
    UTM_ZONA,
    UTM_HEMISFERIO,
    UTM_X,
    UTM_Y,
    LAT,
    LONG,
    TIPO_DE_GEORREFERENCIACION
  ) #%>%

# Spatial validation: remove coordinates outside Paraguay bounds
# dplyr::mutate(
#    LAT  = as.numeric(ifelse(LAT  < -28 | LAT  > -19, NA, LAT)),
#    LONG = as.numeric(ifelse(LONG < -63 | LONG > -54, NA, LONG))
#  )

# ____________________________________________________________
# Logging the transformation
# ____________________________________________________________


add_log(
  step = "process_df_3",
  description = paste0(
    "Processed Aedes albopictus dataset: uppercase conversion, NA normalization, ",
    "ASCII transliteration, year cleaning, disease assignment, species renaming, ",
    "UTM assignment, column selection, and spatial validation."
  ),
  rows_affected = nrow(df_3_tidy),
  variables = names(df_3_tidy)
)

# Optional inspection
#View(df_3_tidy)

# ____________________________________________________________
# 7. Processing of Anopheles dataset (df_4) ----
# ____________________________________________________________

initial_rows_df4 <- nrow(df_4)

df_4_tidy <- df_4 %>%
  
  # Convert all character fields to UPPERCASE
  dplyr::mutate_all(toupper) %>%
  
  # Replace non-informative strings with NA
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/D"))) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "SIN DATOS"))) %>%
  
  # Normalize encoding to ASCII
  dplyr::mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  
  # Standardize species names
  dplyr::mutate(ESPECIE = gsub("^AN$", "ANOPHELES", ESPECIES)) %>%
  dplyr::mutate(ESPECIE = gsub("ANOPHELES  ALBITARSIS,", "ANOPHELES ALBITARSIS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("ANOPHELES  ALBITARSIS", "ANOPHELES ALBITARSIS", ESPECIE)) %>%
  dplyr::mutate(ESPECIE = gsub("ANO\\. ALBITARSIS", "ANOPHELES ALBITARSIS", ESPECIE)) %>%
  
  # Clean year field
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  dplyr::mutate(ANO = na_if(ANO, 0)) %>%
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  
  # Assign disease label
  dplyr::mutate(ENFERMEDAD = "MALARIA") %>%
  
  # Assign UTM metadata
  dplyr::mutate(UTM_ZONA = 21,
                UTM_X = NA,
                UTM_Y = NA) %>%
  
  # Rename coordinate fields
  dplyr::rename(LAT  = LATITU, LONG = LONGITUD) %>%
  
  # Select standardized columns
  dplyr::select(
    DEPARTAMENTO,
    DISTRITO,
    LOCALIDAD,
    ANO,
    FECHA,
    ENFERMEDAD,
    ESPECIE,
    UTM_ZONA,
    UTM_HEMISFERIO,
    UTM_X,
    UTM_Y,
    LAT,
    LONG,
    TIPO_DE_GEORREFERENCIACION
  ) #%>%

# Spatial validation: remove coordinates outside Paraguay bounds
# dplyr::mutate(
#    LAT  = as.numeric(ifelse(LAT  < -28 | LAT  > -19, NA, LAT)),
#    LONG = as.numeric(ifelse(LONG < -63 | LONG > -54, NA, LONG))
#  )

# ____________________________________________________________
# Logging the transformation
# ____________________________________________________________

add_log(
  step = "process_df_4",
  description = paste0(
    "Processed Anopheles dataset: uppercase conversion, NA normalization, ",
    "ASCII transliteration, species name harmonization, year cleaning, ",
    "disease assignment, UTM metadata assignment, coordinate renaming, ",
    "column selection, and spatial validation."
  ),
  rows_affected = nrow(df_4_tidy),
  variables = names(df_4_tidy)
)

# Optional inspection
#View(df_4_tidy)


# ____________________________________________________________
# 8. Processing of Yellow Fever dataset (df_5) ----
# ____________________________________________________________

initial_rows_df5 <- nrow(df_5)

# Ensure required packages
if (!requireNamespace("stringr", quietly = TRUE))
  install.packages("stringr")

# Function to standardize decimal separator to dot and coerce to numeric
sanitize_decimal <- function(x) {
  # If already numeric, return as numeric
  if (is.numeric(x))
    return(as.numeric(x))
  # Convert factors to character
  x_chr <- as.character(x)
  # Trim whitespace
  x_chr <- stringr::str_trim(x_chr)
  # Replace common NA tokens with NA
  x_chr[x_chr %in% c("", "NA", "N/A", "S/D", "SIN DATOS", "S/E")] <- NA_character_
  # Remove thousands separators (either '.' or space) but keep decimal comma/dot
  # First, if both '.' and ',' present, assume '.' thousands and ',' decimal -> remove '.' then replace ','->'.'
  has_dot <- stringr::str_detect(x_chr, "\\.")
  has_comma <- stringr::str_detect(x_chr, ",")
  both <- has_dot & has_comma
  x_chr[both] <- stringr::str_replace_all(x_chr[both], "\\.", "")
  x_chr[both] <- stringr::str_replace_all(x_chr[both], ",", ".")
  # If only comma present, treat comma as decimal separator
  only_comma <- has_comma & !has_dot
  x_chr[only_comma] <- stringr::str_replace_all(x_chr[only_comma], ",", ".")
  # If only dot present, assume dot is decimal (leave as is)
  # Remove any spaces used as thousands separators
  x_chr <- stringr::str_replace_all(x_chr, "\\s+", "")
  # Finally coerce to numeric (suppress warnings for NA coercion)
  suppressWarnings(as.numeric(x_chr))
}

# Function: insert a decimal point after the second digit (ignores sign)
insert_decimal_after_2 <- function(x) {
  # Convert to character and trim
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  # Replace comma decimal with dot, remove thousands separators (spaces or dots used as thousands)
  x_chr <- gsub("\\s+", "", x_chr)
  # If already contains a dot (decimal) leave as is; if contains comma, convert to dot
  has_dot <- grepl("\\.", x_chr)
  x_chr[!has_dot] <- gsub(",", ".", x_chr[!has_dot])
  # Apply regex only to pure integer-like strings (optionally with leading -)
  # Pattern: optional sign, exactly two digits, then one or more digits -> insert dot after the two digits
  x_chr <- ifelse(
    grepl("^[-+]?[0-9]{3,}$", x_chr),
    sub("^([+-]?)([0-9]{2})([0-9]+)$", "\\1\\2.\\3", x_chr),
    x_chr
  )
  # Coerce to numeric (NA if cannot convert)
  suppressWarnings(as.numeric(x_chr))
}

df_5_tidy <- df_5 %>%
  
  # Convert all character fields to UPPERCASE
  dplyr::mutate_all(toupper) %>%
  
  # Replace non-informative strings with NA
  dplyr::mutate_if(is.character, list( ~ na_if(., "S/D"))) %>%
  dplyr::mutate_if(is.character, list( ~ na_if(., "SIN DATOS"))) %>%
  
  # Normalize encoding to ASCII
  dplyr::mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  
  # Clean year field
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  dplyr::mutate(ANO = na_if(ANO, 0)) %>%
  dplyr::mutate(ANO = as.numeric(ANO)) %>%
  
  # Build date field
  dplyr::mutate(FECHA = paste(ANO, MES, DIA, sep = "-")) %>%
  
  # Assign disease label
  dplyr::mutate(ENFERMEDAD = "FIEBRE AMARILLA") %>%
  
  # Rename coordinate and locality fields
  dplyr::rename(LAT       = LATITUDE,
                LONG      = LONGITUDE,
                LOCALIDAD = LOCALITY) %>%
  
  # Assign UTM metadata
  dplyr::mutate(
    UTM_ZONA       = 21,
    UTM_HEMISFERIO = "S",
    UTM_X          = NA,
    UTM_Y          = NA
  ) %>%
  
  # Remove invalid FECHA strings
  dplyr::mutate(FECHA = ifelse(str_detect(FECHA, "NA"), NA_character_, FECHA)) %>%
  
  # Build georeferencing provenance and observation fields
  dplyr::mutate(
    TIPO_DE_GEORREFERENCIACION =
      ifelse(
        !is.na(CANTIDAD) & CANTIDAD != "",
        "DATOS CONSIGNADOS EN CAMPO",
        "DATOS CONSIGNADOS EN GABINETE"
      ),
    
    OBSERVACION =
      ifelse(
        !is.na(LOCALIDAD) & LOCALIDAD != "",
        "CENTROIDE DE LOCALIDAD",
        "CENTROIDE DE DISTRITO"
      ),
    
    TIPO_DE_GEORREFERENCIACION =
      paste0(TIPO_DE_GEORREFERENCIACION, " ", OBSERVACION)
  ) %>%
  
  # Select standardized columns
  dplyr::select(
    DEPARTAMENTO,
    DISTRITO,
    LOCALIDAD,
    ANO,
    FECHA,
    ENFERMEDAD,
    ESPECIE,
    UTM_ZONA,
    UTM_HEMISFERIO,
    UTM_X,
    UTM_Y,
    LAT,
    LONG,
    TIPO_DE_GEORREFERENCIACION
  ) #%>%

# Spatial validation: remove coordinates outside Paraguay bounds
# dplyr::mutate(
#    LAT  = as.numeric(ifelse(LAT  < -28 | LAT  > -19, NA, LAT)),
#    LONG = as.numeric(ifelse(LONG < -63 | LONG > -54, NA, LONG))
#  )

# Example for one dataset (repeat for df_0_tidy ... df_5_tidy)
df_5_tidy <- df_5_tidy %>%
  dplyr::mutate(LAT = sanitize_decimal(LAT), LONG = sanitize_decimal(LONG))  %>%
  mutate(LAT = insert_decimal_after_2(LAT), LONG = insert_decimal_after_2(LONG))


# Log the conversion step
add_log(
  step = "sanitize_decimal_df_5",
  description = "Standardized decimal separator to '.' and coerced LAT/LONG to numeric for df_5_tidy; inserted decimal after 2 digits where needed.",
  rows_affected = nrow(df_5_tidy),
  variables = c("LAT", "LONG", "UTM_X", "UTM_Y")
)


# ____________________________________________________________
# Logging the transformation
# ____________________________________________________________

add_log(
  step = "process_df_5",
  description = paste0(
    "Processed Yellow Fever dataset: uppercase conversion, NA normalization, ",
    "ASCII transliteration, year/date cleaning, disease assignment, coordinate renaming, ",
    "UTM metadata assignment, georeferencing provenance construction, ",
    "decimal sanitization and insertion, column selection, and spatial validation."
  ),
  rows_affected = nrow(df_5_tidy),
  variables = names(df_5_tidy)
)

# Optional inspection
#View(df_5_tidy)

# ____________________________________________________________
# 9. Export individual cleaned datasets (XLSX + CSV) using rio::export
# ____________________________________________________________

# Ensure output folder exists
if (!dir.exists("./data/processed_data"))
  dir.create("./data/processed_data", recursive = TRUE)

file_0 <- "./data/processed_data/Dataset_Leishmaniasis.xlsx"
file_1 <- "./data/processed_data/Dataset_Chagas.xlsx"
file_2 <- "./data/processed_data/Dataset_Dengue.xlsx"
file_3 <- "./data/processed_data/Dataset_Fiebre_Amarilla_Urbana.xlsx"
file_4 <- "./data/processed_data/Dataset_Malaria.xlsx"
file_5 <- "./data/processed_data/Dataset_Fiebre_Amarilla.xlsx"

# Export Excel files (rio detecta formato por extensión) and CSVs
rio::export(df_0_tidy, file_0)
rio::export(df_1_tidy, file_1)
rio::export(df_2_tidy, file_2)
rio::export(df_3_tidy, file_3)
rio::export(df_4_tidy, file_4)
rio::export(df_5_tidy, file_5)

rio::export(df_0_tidy, "./data/processed_data/Dataset_flebotomos.csv")
rio::export(df_1_tidy, "./data/processed_data/Dataset_triatominos.csv")
rio::export(df_2_tidy,
            "./data/processed_data/Dataset_aedes_aegypti.csv")
rio::export(df_3_tidy,
            "./data/processed_data/Dataset_aedes_albopictus.csv")
rio::export(df_4_tidy, "./data/processed_data/Dataset_anopheles.csv")
rio::export(df_5_tidy,
            "./data/processed_data/Dataset_fiebre_amarilla.csv")

add_log(
  step = "export_individual_datasets",
  description = "Exported all cleaned datasets (XLSX + CSV) using rio::export.",
  rows_affected = NA,
  variables = paste0(
    "df_0_tidy,df_1_tidy,df_2_tidy,df_3_tidy,df_4_tidy,df_5_tidy"
  )
)

# ____________________________________________________________
# 10. Merge all datasets into a single consolidated dataset ----
# ____________________________________________________________

# Named list of files to import (use the Excel files created above)
files <- list(
  "Dataset_aedes_albopictus.csv" = file_3,
  "Dataset_anopheles.csv"        = file_4,
  "Dataset_flebotomos.csv"       = file_0,
  "Dataset_triatominos.csv"      = file_1,
  "Dataset_aedes_aegypti.csv"    = file_2,
  "Dataset_fiebre_amarilla.csv"  = file_5
)

# Import each file, add FUENTE, normalize DATE/LAT/LONG types if needed
datasets <- lapply(names(files), function(name) {
  data <- rio::import(files[[name]])
  data$FUENTE <- name
  
  # Normalize FECHA/DATE if present
  if ("FECHA" %in% names(data))
    data$FECHA <- as.character(data$FECHA)
  
  # Ensure LAT/LONG numeric
  if ("LAT" %in% names(data))
    data$LAT <- as.numeric(data$LAT)
  if ("LONG" %in% names(data))
    data$LONG <- as.numeric(data$LONG)
  if ("ANO" %in% names(data))
    data$ANO <- as.numeric(data$ANO)
  return(data)
})


# Combine into one data.frame / tibble
dataset <- dplyr::bind_rows(datasets)


# Example for one dataset (repeat for df_0_tidy ... df_5_tidy)
dataset <- dataset %>%
  dplyr::mutate(LAT = sanitize_decimal(LAT), LONG = sanitize_decimal(LONG))  %>%
  mutate(LAT = insert_decimal_after_2(LAT), LONG = insert_decimal_after_2(LONG))


# Rename to English standardized column names (use backticks for accented names)
dataset_final <- dataset %>%
  dplyr::rename(
    DEPARTMENT = `DEPARTAMENTO`,
    DISTRICT = `DISTRITO`,
    LOCALITY = `LOCALIDAD`,
    YEAR = `ANO`,
    DATE = `FECHA`,
    DISEASE = `ENFERMEDAD`,
    SPECIES = `ESPECIE`,
    UTM_ZONE = `UTM_ZONA`,
    UTM_HEMISPHERE = `UTM_HEMISFERIO`,
    UTM_X = `UTM_X`,
    UTM_Y = `UTM_Y`,
    LAT = `LAT`,
    LONG = `LONG`,
    GEOREFERENCING_TYPE = `TIPO_DE_GEORREFERENCIACION`,
    DATA_SOURCE = FUENTE
  )

add_log(
  step = "merge_datasets",
  description = paste0(
    "Merged all processed datasets using bind_rows() and translated selected column names to English. Final dataset contains ",
    nrow(dataset_final),
    " rows."
  ),
  rows_affected = nrow(dataset_final),
  variables = names(dataset_final)
)

# _____________________________________________________________
# Remove out lat long ----
# _____________________________________________________________

pacman::p_load(sf, rnaturalearth)

# 2) Cargar límite de Paraguay (WGS84)
paraguay <- ne_countries(country = "Paraguay",
                         scale = "medium",
                         returnclass = "sf") %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

# 3) Función segura para filtrar y loguear
filter_dataset_final_paraguay <- function(df,
                                          lat = "LAT",
                                          long = "LONG",
                                          dataset_name = "dataset_final") {
  if (!exists("paraguay"))
    stop("Objeto 'paraguay' no encontrado en el entorno.")
  if (!all(c(lat, long) %in% names(df))) {
    add_log(
      step = paste0("filter_", dataset_name, "_skip"),
      description = paste(
        "Columnas",
        lat,
        "y/o",
        long,
        "no encontradas; no se aplicó filtro espacial."
      ),
      rows_affected = nrow(df),
      variables = names(df)
    )
    stop("Columnas LAT/LONG no encontradas en el data.frame.")
  }
  # Coerción numérica defensiva
  df2 <- df %>%
    mutate(!!lat := suppressWarnings(as.numeric(.data[[lat]])),!!long := suppressWarnings(as.numeric(.data[[long]]))) %>%
    filter(!is.na(.data[[lat]]) & !is.na(.data[[long]]))
  initial_n <- nrow(df)
  coords_n <- nrow(df2)
  # Detectar e invertir si LAT fuera de rango
  if (any(abs(df2[[lat]]) > 90, na.rm = TRUE) &&
      all(abs(df2[[long]]) <= 90, na.rm = TRUE)) {
    df2 <- df2 %>% rename(.tmp_lat = !!sym(lat), !!lat := !!sym(long), !!long := .tmp_lat) %>% select(-.tmp_lat)
    add_log(
      step = paste0("swap_latlong_", dataset_name),
      description = "Se detectó LAT fuera de rango; se intercambiaron LAT/LONG.",
      rows_affected = nrow(df2),
      variables = c(lat, long)
    )
  }
  # Crear sf y evaluar pertenencia
  pts_sf <- st_as_sf(df2,
                     coords = c(long, lat),
                     crs = 4326,
                     remove = FALSE)
  if (st_crs(pts_sf) != st_crs(paraguay))
    pts_sf <- st_transform(pts_sf, st_crs(paraguay))
  inside <- lengths(st_within(pts_sf, paraguay)) > 0
  kept_df <- pts_sf[inside, ] %>% st_drop_geometry()
  removed_df <- pts_sf[!inside, ] %>% st_drop_geometry()
  # Log
  add_log(
    step = paste0("filter_paraguay_", dataset_name),
    description = paste0(
      "Filtro espacial aplicado: inicial=",
      initial_n,
      "; con coordenadas=",
      coords_n,
      "; retenidos=",
      nrow(kept_df),
      "; eliminados=",
      nrow(removed_df),
      "."
    ),
    rows_affected = nrow(kept_df),
    variables = c(lat, long)
  )
  list(kept = kept_df, removed = removed_df)
}

# 4) Aplicar sobre dataset_final y actualizar entorno
res <- filter_dataset_final_paraguay(dataset_final,
                                     lat = "LAT",
                                     long = "LONG",
                                     dataset_name = "dataset_final")
dataset_final <- res$kept
dataset_final_removed <- res$removed

# 5) Registrar resultados finales
add_log(
  step = "finalize_filter_dataset_final",
  description = paste0(
    "dataset_final actualizado; registros válidos: ",
    nrow(dataset_final),
    "; registros eliminados: ",
    nrow(dataset_final_removed)
  ),
  rows_affected = nrow(dataset_final),
  variables = c("LAT", "LONG")
)

# 11. Taxonomic corrections ----

# Cargar tabla de referencia taxonómica 


clean_species_names <- function(x) {
  x %>%
    toupper() %>%
    # eliminar cadenas problemáticas
    stringr::str_replace_all("\\(COMPLEJO\\)", "") %>%
    stringr::str_replace_all("\\bCOMPLEJO\\b", "") %>%
    stringr::str_replace_all("\\(COMPLEX\\)", "") %>%
    stringr::str_replace_all("\\bCOMPLEX\\b", "") %>%
    stringr::str_replace_all("\\(ARRIB\\)", "") %>%
    stringr::str_replace_all("SP\\.\\.", "") %>%
    stringr::str_replace_all("SP\\.", "") %>%
    stringr::str_replace_all("\\bSP\\b", "") %>%
    stringr::str_replace_all("0 0", "") %>%
    stringr::str_replace_all("[()]", "") %>%
    
    # corrección específica solicitada
    stringr::str_replace_all("^ANPHELES ALBITARSIS$", "ANOPHELES ALBITARSIS") %>%
    # corrección más general (por si aparece ANPHELES en otras combinaciones)
    stringr::str_replace_all("^ANPHELES", "ANOPHELES") %>%
    stringr::str_replace_all("^NA SHANNONI$", "PSATHYROMYIA SHANNONI") %>%
    stringr::str_replace_all("^PSATIROMYIA SHANNONI$", "PSATHYROMYIA SHANNONI") %>%
    stringr::str_replace_all("^ANOPHELES ROOZEBOMII$", "NYSSORHYNCHUS ROZEBOOMI") %>% #Anopheles rozeboomi
    stringr::str_replace_all("^P.PANSTRONGYLUS GENICULATUS$", "PANSTRONGYLUS GENICULATUS") %>% #Panstrongylus geniculatus
    # limpieza final
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\.+$", "")
}



dataset_final_aux <- dataset_final %>%
  dplyr::rename(SPECIES_RAW = SPECIES) %>%
  dplyr::mutate(
    SPECIES_RAW = toupper(stringr::str_trim(SPECIES_RAW)),
    DISEASE     = toupper(stringr::str_trim(DISEASE)))%>%
      dplyr::mutate(SPECIES_CLEAN = clean_species_names(SPECIES_RAW))
   

add_log(
  step = "taxonomic_string_cleaning",
  description = paste0(
    "Applied taxonomic string cleaning rules to SPECIES_RAW: ",
    "removed unwanted substrings (COMPLEJO, COMPLEX, ARRIB, SP.., SP., SP, 0 0, parentheses), ",
    "standardized uppercase formatting, normalized whitespace, and applied specific corrections: ",
    "ANPHELES→ANOPHELES, ANPHELES ALBITARSIS→ANOPHELES ALBITARSIS, ",
    "NA SHANNONI→PSATHYROMYIA SHANNONI, PSATIROMYIA SHANNONI→PSATHYROMYIA SHANNONI, ",
    "ANOPHELES ROOZEBOMII→NYSSORHYNCHUS ROZEBOOMI, ",
    "P.PANSTRONGYLUS GENICULATUS→PANSTRONGYLUS GENICULATUS. ",
    "Generated SPECIES_CLEAN as standardized taxonomic field."
  ),
  rows_affected = nrow(dataset_final_aux),
  variables = c("SPECIES_RAW", "SPECIES_CLEAN")
)


# Función robusta para resolver nombres con GBIF

resolve_species_gbif <- function(species_vec) {
  spp_unique <- species_vec %>%
    unique() %>%
    na.omit() %>%
    stringr::str_trim() %>%
    toupper()
  
  out <- purrr::map_df(spp_unique, function(x) {
    r <- tryCatch(
      rgbif::name_backbone(name = x),
      error = function(e) NULL
    )
    
    tibble::tibble(
      SPECIES_CLEAN = x,
      SPECIES_MATCHED = r$scientificName %||% NA_character_,
      STATUS = r$status %||% NA_character_,
      CONFIDENCE = r$confidence %||% NA_real_,
      KINGDOM = r$kingdom %||% NA_character_,
      PHYLUM = r$phylum %||% NA_character_,
      ORDER = r$order %||% NA_character_,
      FAMILY = r$family %||% NA_character_,
      GENUS = r$genus %||% NA_character_,
      SPECIES = r$species %||% NA_character_
    )
  })
  
  add_log(
    step = "gbif_name_backbone",
    description = paste0(
      "Resolved species names using GBIF backbone. ",
      "Returned ", nrow(out), " rows."
    ),
    rows_affected = nrow(out),
    variables = names(out)
  )
  
  return(out)
}

taxize_map <- resolve_species_gbif(dataset_final_aux$SPECIES_CLEAN)

add_log(
  step = "taxonomic_resolution_gbif",
  description = paste0(
    "Resolved cleaned species names using GBIF Backbone Taxonomy. ",
    "Generated taxonomic mapping table (taxize_map) including accepted names, status, confidence scores, ",
    "and higher-level taxonomy (kingdom, phylum, order, family, genus, species). ",
    "This table will be used for harmonization and QA of taxonomic nomenclature."
  ),
  rows_affected = nrow(taxize_map),
  variables = names(taxize_map)
)


dataset_final_gbif_map <- dataset_final_aux %>%
  left_join(taxize_map, by = "SPECIES_CLEAN")

add_log(
  step = "apply_taxonomic_mapping",
  description = paste0(
    "Integrated GBIF-resolved taxonomy into dataset_final_aux. ",
    "Added SPECIES_MATCHED, STATUS, CONFIDENCE, and higher taxonomy fields. ",
    "Created harmonized species nomenclature for downstream analyses and QA."
  ),
  rows_affected = nrow(dataset_final_aux),
  variables = c("SPECIES_CLEAN", "SPECIES_MATCHED", "STATUS", "CONFIDENCE")
)

#export(dataset_final_gbif_map,"data/processed_data/dataset_final_gbif_map.xlsx")

taxize_map %>% drop_na(SPECIES_CLEAN) %>% 
rio::export("data/processed_data/gbif_map.xlsx")

# Import corrected taxonomy data with AI-based app Gemini

tax_AI = rio::import("data/processed_data/AI_Assistant_map_TaxoConsensus_Pro_app.xlsx")

dataset_final  = dataset_final_gbif_map %>% dplyr::left_join(tax_AI, by = "SPECIES_CLEAN")

add_log(
  step = "import_taxonomy_AI_map",
  description = paste0(
    "Imported AI-generated taxonomic consensus table (AI_Assistant_map_TaxoConsensus_Pro_app.xlsx) ",
    "and integrated it into dataset_final_gbif_map via left_join using SPECIES_CLEAN as key. ",
    "This step incorporates AI-assisted corrections, synonym harmonization, and consensus nomenclature ",
    "into the unified dataset, complementing GBIF backbone resolution and manual string cleaning."
  ),
  rows_affected = nrow(dataset_final),
  variables = c("SPECIES_CLEAN", names(tax_AI))
)

# ____________________________________________________________
# 11. Export final combined dataset (XLSX + CSV) using rio::export
# ____________________________________________________________

# Ensure output folder exists
if (!dir.exists("./data/processed_data"))
  dir.create("./data/processed_data", recursive = TRUE)

rio::export(dataset_final,
            "./data/processed_data/SENEPA_tidy_data_set.xlsx")
rio::export(dataset_final,
            "./data/processed_data/SENEPA_tidy_data_set.csv")

add_log(
  step = "export_combined_dataset",
  description = "Exported final merged dataset (XLSX + CSV) using rio::export.",
  rows_affected = nrow(dataset_final),
  variables = names(dataset_final)
)


# Save processing_log (UTF-8)
if (!dir.exists("./logs"))
  dir.create("./logs", recursive = TRUE)
# prefer readr::write_csv for reliable UTF-8; rio::export can also be used for CSV but readr is explicit
readr::write_csv(processing_log, "./logs/processing_log.csv")

write.csv(
  processing_log,
  "./logs/processing_log_latin1.csv",
  fileEncoding = "Latin1",
  row.names = FALSE
)

# Capturar y exportar sessionInfo() a archivos (texto y RDS) y registrar con add_log
# Ajusta la carpeta de salida si hace falta
out_dir <- "logs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Nombre de archivo con fecha-hora
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
txt_file <- file.path(out_dir, paste0("sessionInfo_", ts, ".txt"))
rds_file <- file.path(out_dir, paste0("sessionInfo_", ts, ".rds"))

# 1) Guardar salida legible en texto
capture.output(sessionInfo(), file = txt_file)

# 2) Guardar el objeto sessionInfo como RDS para inspección programática
saveRDS(sessionInfo(), file = rds_file)

# 3) (Opcional) imprimir en consola
cat("sessionInfo saved to:\n", txt_file, "\n", rds_file, "\n")

# 4) Registrar en processing_log si existe add_log
if (exists("add_log")) {
  add_log(
    step = "export_sessionInfo",
    description = paste0("Exported sessionInfo to files: ", basename(txt_file), ", ", basename(rds_file)),
    rows_affected = NA_integer_,
    variables = c("R.version", "loadedNamespaces")
  )
}

qa_report <- function(
    df,
    tax_map = NULL,
    tax_ai = NULL,
    out_dir = "./logs/qa",
    name_prefix = "qa",
    year_range = c(1900, as.integer(format(Sys.Date(), "%Y"))),
    gbif_conf_threshold = 80,
    paraguay_bbox = list(lat_min = -28, lat_max = -19, long_min = -63, long_max = -54),
    add_log = add_log
) {
  pacman::p_load(dplyr, stringr, janitor, readr, openxlsx, tibble, sf, lubridate)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Defensive rename to uppercase columns
  df <- df %>% rename_with(~ toupper(.x))
  cols <- names(df)
  n_total <- nrow(df)
  
  # Basic overview metrics
  n_unique_species <- df %>% pull(SPECIES_CLEAN) %>% unique() %>% length()
  add_log("qa_start", "Started QA report generation", n_total, c("SPECIES_CLEAN"))
  
  # Date parsing and year checks
  df <- df %>%
    mutate(
      DATE2 = suppressWarnings(as.Date(DATE)),
      YEAR2 = ifelse(!is.na(YEAR), as.integer(YEAR), lubridate::year(DATE2))
    )
  bad_dates <- df %>% filter((!is.na(DATE) & is.na(DATE2)) |
                               (!is.na(YEAR2) & (YEAR2 < year_range[1] | YEAR2 > year_range[2])))
  
  add_log("qa_dates", paste0("Checked dates. Invalid or out of range: ", nrow(bad_dates)),
          nrow(bad_dates), c("DATE","YEAR"))
  
  # Coordinates numeric coercion and basic bounds
  df <- df %>% mutate(
    LATn = suppressWarnings(as.numeric(LAT)),
    LONGn = suppressWarnings(as.numeric(LONG))
  )
  
  bad_coords <- df %>%
    filter((!is.na(LATn) & (LATn < paraguay_bbox$lat_min | LATn > paraguay_bbox$lat_max)) |
             (!is.na(LONGn) & (LONGn < paraguay_bbox$long_min | LONGn > paraguay_bbox$long_max)))
  
  # Detect possible lat/long swap
  swap_candidates <- df %>%
    filter(!is.na(LATn) & !is.na(LONGn)) %>%
    filter(abs(LATn) <= 90 & abs(LONGn) <= 90) %>%
    filter(LATn > 0 & LONGn < 0) # heuristic: positive lat in Paraguay unlikely
  
  add_log("qa_coords", paste0("Coords checks: out_of_bounds=", nrow(bad_coords),
                              "; swap_candidates=", nrow(swap_candidates)),
          nrow(df), c("LAT","LONG"))
  
  # Spatial containment using sf if available
  paraguay_sf <- tryCatch({
    rnaturalearth::ne_countries(country = "Paraguay", scale = "medium", returnclass = "sf") %>%
      sf::st_make_valid() %>% sf::st_transform(crs = 4326)
  }, error = function(e) NULL)
  
  inside_paraguay <- tibble::tibble()
  if (!is.null(paraguay_sf) && all(c("LATn","LONGn") %in% names(df))) {
    pts <- df %>% filter(!is.na(LATn) & !is.na(LONGn)) %>%
      st_as_sf(coords = c("LONGn","LATn"), crs = 4326, remove = FALSE)
    inside <- sf::st_within(pts, paraguay_sf)
    pts$inside_paraguay <- lengths(inside) > 0
    inside_paraguay <- pts %>% st_drop_geometry() %>% select(DEPARTMENT, DISTRICT, LOCALITY, DATE, SPECIES_CLEAN, LATn, LONGn, inside_paraguay)
    add_log("qa_spatial_within", paste0("Spatial containment checked; points with coords: ", nrow(pts)), nrow(pts), c("LATn","LONGn"))
  }
  
  # Duplicates exact and spatial approximate
  key_cols <- c("DEPARTMENT","DISTRICT","LOCALITY","DATE","SPECIES_CLEAN")
  exact_dups <- if (all(key_cols %in% names(df))) {
    df %>% group_by(across(all_of(key_cols))) %>% tally() %>% filter(n > 1) %>% arrange(desc(n))
  } else tibble::tibble()
  
  # Taxonomic coverage and confidence
  has_gbif <- "SPECIES_MATCHED" %in% cols
  species_no_gbif <- if (has_gbif) df %>% filter(is.na(SPECIES_MATCHED) | SPECIES_MATCHED == "") %>% distinct(SPECIES_CLEAN) else tibble::tibble()
  low_conf <- if ("CONFIDENCE" %in% cols) df %>% filter(!is.na(CONFIDENCE) & CONFIDENCE < gbif_conf_threshold) %>% distinct(SPECIES_CLEAN, CONFIDENCE) else tibble::tibble()
  
  add_log("qa_taxonomy", paste0("Taxonomy checks: no_gbif=", nrow(species_no_gbif), "; low_conf=", nrow(low_conf)),
          nrow(df), c("SPECIES_CLEAN","SPECIES_MATCHED","CONFIDENCE"))
  
  # Binomial completeness
  df <- df %>% mutate(
    GENUS = stringr::word(SPECIES_CLEAN, 1),
    EPITHET = stringr::word(SPECIES_CLEAN, 2)
  )
  incomplete_binomial <- df %>% filter(is.na(EPITHET) | EPITHET == "") %>% distinct(SPECIES_CLEAN)
  
  # Compare with AI map if provided
  compare_ai <- tibble::tibble()
  if (!is.null(tax_ai)) {
    tax_ai <- tax_ai %>% rename_with(~ toupper(.x))
    if ("SPECIES_CLEAN" %in% names(tax_ai)) {
      compare_ai <- df %>% distinct(SPECIES_CLEAN) %>%
        left_join(tax_ai %>% distinct(SPECIES_CLEAN) %>% mutate(IN_AI = TRUE), by = "SPECIES_CLEAN") %>%
        mutate(IN_AI = ifelse(is.na(IN_AI), FALSE, TRUE))
      add_log("qa_compare_ai", paste0("Compared species list with AI map; AI matches: ", sum(compare_ai$IN_AI)), nrow(compare_ai), c("SPECIES_CLEAN"))
    }
  }
  
  # Summary by disease and species
  summary_by_disease <- df %>% group_by(DISEASE, SPECIES_CLEAN) %>% summarise(N = n(), .groups = "drop") %>% arrange(DISEASE, desc(N))
  
  # Export results to XLSX and CSV
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_xlsx <- file.path(out_dir, paste0(name_prefix, "_report_", ts, ".xlsx"))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "overview")
  openxlsx::writeData(wb, "overview", tibble::tibble(total_rows = n_total, unique_species = n_unique_species))
  openxlsx::addWorksheet(wb, "bad_dates"); openxlsx::writeData(wb, "bad_dates", bad_dates %>% select(DEPARTMENT, DISTRICT, LOCALITY, DATE, YEAR2, SPECIES_CLEAN))
  openxlsx::addWorksheet(wb, "bad_coords"); openxlsx::writeData(wb, "bad_coords", bad_coords %>% select(DEPARTMENT, DISTRICT, LOCALITY, LATn, LONGn, SPECIES_CLEAN))
  openxlsx::addWorksheet(wb, "swap_candidates"); openxlsx::writeData(wb, "swap_candidates", swap_candidates %>% select(DEPARTMENT, DISTRICT, LOCALITY, LATn, LONGn, SPECIES_CLEAN))
  openxlsx::addWorksheet(wb, "exact_dups"); openxlsx::writeData(wb, "exact_dups", exact_dups)
  openxlsx::addWorksheet(wb, "no_gbif"); openxlsx::writeData(wb, "no_gbif", species_no_gbif)
  openxlsx::addWorksheet(wb, "low_conf"); openxlsx::writeData(wb, "low_conf", low_conf)
  openxlsx::addWorksheet(wb, "incomplete_binomial"); openxlsx::writeData(wb, "incomplete_binomial", incomplete_binomial)
  openxlsx::addWorksheet(wb, "by_disease"); openxlsx::writeData(wb, "by_disease", summary_by_disease)
  if (nrow(compare_ai) > 0) { openxlsx::addWorksheet(wb, "compare_ai"); openxlsx::writeData(wb, "compare_ai", compare_ai) }
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  
  # CSV quick flags
  write_csv(bad_dates %>% select(DEPARTMENT, DISTRICT, LOCALITY, DATE, SPECIES_CLEAN), file.path(out_dir, paste0(name_prefix, "_bad_dates_", ts, ".csv")))
  write_csv(bad_coords %>% select(DEPARTMENT, DISTRICT, LOCALITY, LATn, LONGn, SPECIES_CLEAN), file.path(out_dir, paste0(name_prefix, "_bad_coords_", ts, ".csv")))
  write_csv(species_no_gbif, file.path(out_dir, paste0(name_prefix, "_no_gbif_", ts, ".csv")))
  write_csv(low_conf, file.path(out_dir, paste0(name_prefix, "_low_conf_", ts, ".csv")))
  
  # Metadata and session info
  qa_meta <- tibble::tibble(
    report_generated = Sys.time(),
    dataset_rows = n_total,
    report_path = out_xlsx,
    operator = Sys.getenv("USER", unset = "unknown")
  )
  write_csv(qa_meta, file.path(out_dir, paste0(name_prefix, "_metadata_", ts, ".csv")))
  write_lines(capture.output(sessionInfo()), file.path(out_dir, paste0(name_prefix, "_sessioninfo_", ts, ".txt")))
  
  # Final logs
  add_log("qa_report_generate", paste0("QA report generated and exported to: ", out_xlsx), n_total, c("SPECIES_CLEAN","DATE","LAT","LONG"))
  add_log("qa_finish", "QA report completed", n_total, c("overview","bad_dates","bad_coords"))
  
  # Return programmatic object
  invisible(list(
    overview = tibble::tibble(total_rows = n_total, unique_species = n_unique_species),
    bad_dates = bad_dates,
    bad_coords = bad_coords,
    swap_candidates = swap_candidates,
    exact_dups = exact_dups,
    species_no_gbif = species_no_gbif,
    low_conf = low_conf,
    incomplete_binomial = incomplete_binomial,
    summary_by_disease = summary_by_disease,
    compare_ai = compare_ai,
    out_xlsx = out_xlsx,
    meta = qa_meta
  ))
}


res_qa <- qa_report(
  df = dataset_final,
  tax_map = taxize_map,
  tax_ai = tax_AI,
  out_dir = "./logs/qa",
  name_prefix = "SENEPA_taxonomic_QA",
  year_range = c(1950, 2026),
  gbif_conf_threshold = 80,
  add_log = add_log
)

add_log(
  step = "qa_integration",
  description = paste0("QA report executed; outputs: ", res_qa$out_xlsx),
  rows_affected = nrow(dataset_final),
  variables = c("SPECIES_CLEAN","SPECIES_MATCHED","CONFIDENCE","LAT","LONG","DATE")
)
