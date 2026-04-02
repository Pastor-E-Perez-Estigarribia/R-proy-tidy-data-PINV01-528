impute_date_from_year <- function(
    df,
    year_col = "YEAR",
    date_col = "DATE",
    out_col = "DATE_FINAL",
    default_month = 1,
    default_day = 1,
    prefer_midyear = FALSE,
    add_log = NULL
) {
  pacman::p_load(dplyr, stringr, lubridate, purrr, tibble, rlang, glue)
  
  # Normalize column names
  df <- df %>% rename_with(~ toupper(.x))
  year_col <- toupper(year_col)
  date_col <- toupper(date_col)
  out_col  <- toupper(out_col)
  
  if (!date_col %in% names(df)) stop(glue::glue("Column '{date_col}' not found in df"))
  if (!year_col %in% names(df)) df[[year_col]] <- NA_character_
  
  # Robust date parser
  safe_parse_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    x_chr <- as.character(x)
    x_chr[is.na(x_chr) | x_chr == ""] <- NA_character_
    orders <- c("Ymd", "Y-m-d", "Y/m/d", "dmY", "dmy", "ymd HMS", "YmdT")
    out <- suppressWarnings(lubridate::parse_date_time(x_chr, orders = orders, exact = FALSE))
    as.Date(out)
  }
  
  # Extract 4-digit year
  extract_year <- function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr) | x_chr == ""] <- NA_character_
    yr <- stringr::str_extract(x_chr, "\\d{4}")
    yr_num <- suppressWarnings(as.integer(yr))
    yr_num[!is.na(yr_num) & (yr_num < 1800 | yr_num > 9999)] <- NA_integer_
    return(yr_num)
  }
  
  # default month/day
  if (isTRUE(prefer_midyear)) {
    default_month <- 6L; default_day <- 30L
  } else {
    default_month <- as.integer(default_month); default_day <- as.integer(default_day)
  }
  
  # parse existing DATE and extract year
  df <- df %>%
    mutate(
      .DATE_PARSED = safe_parse_date(.data[[date_col]]),
      .YEAR_EXTRACT = extract_year(.data[[year_col]])
    )
  
  # build_from_year returns Date or NA
  build_from_year <- function(y) {
    if (is.na(y)) return(NA_Date_)
    y <- as.integer(y)
    if (is.na(y) || y < 1800 || y > 9999) return(NA_Date_)
    tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, default_month, default_day)), error = function(e) NA_Date_)
  }
  
  # Create a proper Date vector for fallback using vapply (not a list)
  fallback_dates <- vapply(df$.YEAR_EXTRACT, FUN = function(y) build_from_year(y), FUN.VALUE = as.Date("1970-01-01"))
  
  # Now coalesce with atomic Date vectors
  df <- df %>%
    mutate(
      !!out_col := coalesce(.DATE_PARSED, as.Date(fallback_dates, origin = "1970-01-01")),
      DATE_STR = if_else(!is.na(!!rlang::sym(out_col)), format(!!rlang::sym(out_col), "%Y-%m-%d"), NA_character_),
      MONTH_STR = if_else(!is.na(!!rlang::sym(out_col)), format(floor_date(!!rlang::sym(out_col), "month"), "%Y-%m"), NA_character_)
    ) %>%
    select(-.DATE_PARSED, -.YEAR_EXTRACT)
  
  # Logging
  n_total <- nrow(df)
  n_imputed <- df %>% filter(is.na(.data[[date_col]]) & !is.na(.data[[out_col]])) %>% nrow()
  if (!is.null(add_log) && is.function(add_log)) {
    add_log(
      step = "impute_date_from_year_v3",
      description = paste0(
        "Imputed missing DATE from YEAR where possible. Default imputation uses ",
        sprintf("%02d-%02d", default_month, default_day),
        ifelse(prefer_midyear, " (mid-year preferred).", ".")
      ),
      rows_affected = n_imputed,
      variables = c(date_col, year_col, out_col)
    )
  }
  
  return(df)
}