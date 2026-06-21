# Instalar paquetes si no existen
# install.packages(c("shiny", "bslib", "DT", "httr", "jsonlite", "dplyr", "markdown", "shinymanager", "RSQLite", "DBI"))

require('pacman')

# if (!require('pacman'))
#   install.packages("pacman")
# library(pacman)


pacman::p_load(
shiny,
bslib,
DT,
httr,
jsonlite,
dplyr,
markdown,
shinymanager,
RSQLite,
DBI)

# --- 1. CONFIGURACIÓN BASE DE DATOS Y USUARIOS ---
# Credenciales para el Login
credentials <- data.frame(
  user = c("evaluador", "admin"), # Usuarios
  password = c("revisor2026", "admin123"), # Contraseñas
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Inicializar Base de Datos SQLite para Logs
con <- dbConnect(RSQLite::SQLite(), "logs_consumo.sqlite")
if (!dbExistsTable(con, "api_logs")) {
  dbExecute(con, "CREATE TABLE api_logs (user TEXT, timestamp TEXT, action TEXT, target TEXT)")
}

# --- 2. DICCIONARIO DE TRADUCCIONES (i18n) ---
i18n <- list(
  es = list(
    title = "TaxoConsensus Hub 🔬",
    config = "Configuración",
    lang = "Idioma / Language",
    filter = "Filtrar por Estatus:",
    btn_global = "✨ Informe Global IA",
    tot_reg = "Total Registros",
    conf_mean = "Confianza Media",
    synonyms = "Sinónimos",
    ai_title = "Asistente de IA (Resultados)",
    table_title = "Explorador de Vectores",
    empty_ai = "Seleccione una especie o genere el Informe Global.",
    analyzing = "Analizando...",
    generating = "Generando Informe..."
  ),
  en = list(
    title = "TaxoConsensus Hub 🔬",
    config = "Configuration",
    lang = "Language / Idioma",
    filter = "Filter by Status:",
    btn_global = "✨ Global AI Report",
    tot_reg = "Total Records",
    conf_mean = "Avg Confidence",
    synonyms = "Synonyms",
    ai_title = "AI Assistant (Results)",
    table_title = "Vector Explorer",
    empty_ai = "Select a species or generate the Global Report.",
    analyzing = "Analyzing...",
    generating = "Generating Report..."
  )
)

# --- 3. DATASET DE EJEMPLO ---
taxonomy_data <- data.frame(
  disease = c("FIEBRE AMARILLA / ZIKA", "MALARIA", "MALARIA", "MALARIA", "MALARIA"),
  clean = c("AEDES ALBOPICTUS", "ANOPHELES ALBITARSIS", "ANOPHELES NOROESTENSIS", "ANOPHELES DARLINGI", "ANOPHELES EVANSAE"),
  matched = c("Aedes albopictus (Skuse, 1894)", "Anopheles albitarsis Lynch Arribálzaga, 1878", "Anopheles noroestensis Galvão & Lane, 1937", "Anopheles darlingi Root, 1926", "Anopheles evansae (Brèthes, 1926)"),
  conf = c(99, 97, 98, 99, 99),
  status = c("ACCEPTED", "ACCEPTED", "SYNONYM", "ACCEPTED", "ACCEPTED"),
  stringsAsFactors = FALSE
)

PROJECT_ID <- "PINV01-528"

# --- 4. FUNCIÓN LLAMADA GEMINI ---
call_gemini <- function(prompt, system_prompt) {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (api_key == "") stop("API Key no configurada en el servidor.")
  
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-lite:generateContent?key=", api_key)
  body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    systemInstruction = list(parts = list(list(text = system_prompt))),
    generationConfig = list(temperature = 0.1)
  )
  res <- POST(url, body = body, encode = "json")
  content <- content(res, "parsed")
  if (!is.null(content$error)) stop(content$error$message)
  return(content$candidates[[1]]$content$parts[[1]]$text)
}

# --- 5. INTERFAZ DE USUARIO (Envuelto en secure_app) ---
ui <- secure_app(
  uiOutput("dynamic_ui"), # UI Dinámica para permitir el cambio de idioma
  language = "es" # Idioma de la pantalla de login
)

# --- 6. LÓGICA DEL SERVIDOR ---
server <- function(input, output, session) {
  
  # Autenticación
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # Estado reactivo para el idioma (por defecto español)
  current_lang <- reactiveVal("es")
  ai_response <- reactiveVal("")
  
  # Observar cambio de idioma
  observeEvent(input$lang_selector, {
    current_lang(input$lang_selector)
    if (ai_response() == "") {
      ai_response(paste0("<div style='text-align:center; padding: 20px; color: #6c757d;'>", i18n[[input$lang_selector]]$empty_ai, "</div>"))
    }
  })
  
  # Generación Dinámica de la Interfaz (para aplicar traducciones)
  output$dynamic_ui <- renderUI({
    t <- i18n[[current_lang()]]
    
    page_sidebar(
      title = t$title,
      theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#4f46e5"),
      
      sidebar = sidebar(
        title = t$config,
        selectInput("lang_selector", t$lang, choices = c("Español" = "es", "English" = "en"), selected = current_lang()),
        hr(),
        selectInput("status_filter", t$filter, choices = c("ALL", "ACCEPTED", "SYNONYM")),
        hr(),
        actionButton("btn_global", t$btn_global, class = "btn-primary w-100"),
        p(class = "text-muted mt-4", style = "font-size: 0.8em;", paste("Usuario logueado:", res_auth$user))
      ),
      
      layout_columns(
        fill = FALSE,
        value_box(title = t$tot_reg, value = nrow(taxonomy_data), showcase = bsicons::bs_icon("list-ul")),
        value_box(title = t$conf_mean, value = paste0(round(mean(taxonomy_data$conf), 1), "%"), showcase = bsicons::bs_icon("check-circle")),
        value_box(title = t$synonyms, value = sum(taxonomy_data$status == "SYNONYM"), showcase = bsicons::bs_icon("exclamation-triangle"))
      ),
      
      card(full_screen = TRUE, card_header(t$ai_title), uiOutput("ai_result_ui")),
      card(full_screen = TRUE, card_header(t$table_title), DTOutput("vector_table"))
    )
  })
  
  # Filtro de Tabla
  filtered_data <- reactive({
    if (is.null(input$status_filter) || input$status_filter == "ALL") taxonomy_data else taxonomy_data %>% filter(status == input$status_filter)
  })
  
  output$vector_table <- renderDT({
    datatable(filtered_data(), selection = "single", options = list(pageLength = 5, dom = 'ftip'),
              rownames = FALSE, colnames = c("Disease", "Species (Clean)", "Match GBIF", "Confidence (%)", "Status")) %>%
      formatStyle('conf', backgroundColor = styleInterval(94, c('#fef3c7', '#d1fae5')))
  })
  
  # Acción: Seleccionar Fila
  observeEvent(input$vector_table_rows_selected, {
    req(input$vector_table_rows_selected)
    t <- i18n[[current_lang()]]
    
    selected_row <- filtered_data()[input$vector_table_rows_selected, ]
    species <- selected_row$clean
    
    # LOG EN SQLITE
    dbExecute(con, "INSERT INTO api_logs (user, timestamp, action, target) VALUES (?, ?, ?, ?)", 
              params = list(res_auth$user, as.character(Sys.time()), "Especie", species))
    
    ai_response(paste0("<div style='text-align:center;'><div class='spinner-border text-primary'></div><p>", t$analyzing, "</p></div>"))
    
    sys_prompt <- paste0("Eres un experto entomólogo del proyecto ", PROJECT_ID, ". Resuelve estatus de especie: 1. Correct Name, 2. Justification, 3. Medical Importance. ESTRICTAMENTE PROHIBIDO: taxonIDs ni LSIDs. Idioma: ", ifelse(current_lang()=="es", "Español", "English"), ". Formato Markdown.")
    
    tryCatch({
      result <- call_gemini(paste("Resuelve:", species), sys_prompt)
      ai_response(markdown::markdownToHTML(text = result, fragment.only = TRUE))
    }, error = function(e) ai_response(paste("<div style='color:red;'><b>Error:</b>", e$message, "</div>")))
  })
  
  # Acción: Informe Global
  observeEvent(input$btn_global, {
    t <- i18n[[current_lang()]]
    
    # LOG EN SQLITE
    dbExecute(con, "INSERT INTO api_logs (user, timestamp, action, target) VALUES (?, ?, ?, ?)", 
              params = list(res_auth$user, as.character(Sys.time()), "Global_Report", "All Vectors"))
    
    ai_response(paste0("<div style='text-align:center;'><div class='spinner-border text-primary'></div><p>", t$generating, "</p></div>"))
    sys_prompt <- paste0("Eres un epidemiólogo experto. Proyecto: ", PROJECT_ID, ". Genera un Informe de Riesgo profesional. SIN códigos LSID. Idioma: ", ifelse(current_lang()=="es", "Español", "English"), ". Formato Markdown.")
    
    tryCatch({
      result <- call_gemini(paste("Dataset:", paste(taxonomy_data$clean, collapse = ", ")), sys_prompt)
      ai_response(markdown::markdownToHTML(text = result, fragment.only = TRUE))
    }, error = function(e) ai_response(paste("<div style='color:red;'><b>Error:</b>", e$message, "</div>")))
  })
  
  output$ai_result_ui <- renderUI({ HTML(ai_response()) })
}

# Cerrar conexión a BD al apagar la app
onStop(function() { dbDisconnect(con) })

shinyApp(ui, server)