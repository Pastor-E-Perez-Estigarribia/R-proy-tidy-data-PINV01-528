# Instalar paquetes si no existen
# install.packages(c("pacman", "shiny", "bslib", "DT", "httr", "jsonlite", "dplyr", "markdown", "shinymanager", "RSQLite", "DBI"))

require('pacman')

pacman::p_load(
  rio,
  tidyverse,
  shiny,
  bslib,
  DT,
  httr,
  jsonlite,
  dplyr,
  markdown,
  shinymanager,
  RSQLite,
  DBI
)

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
    api_label = "Tu API Key de Gemini (Opcional):",
    api_ph = "Deja en blanco para usar cuota pública...",
    api_help = "👉 Obtén tu API Key gratuita aquí",
    lang = "Idioma / Language",
    filter = "Filtrar por Estatus:",
    custom_search = "Consultar especie específica:",
    btn_custom = "🔍 Analizar Especie",
    tot_reg = "Total Registros",
    ai_title = "Asistente de IA (Resultados)",
    table_title = "Explorador de Vectores",
    empty_ai = "Seleccione una especie en la tabla o busque una manualmente.",
    analyzing = "Analizando...",
    error_empty_search = "Por favor, ingrese el nombre de un taxón primero."
  ),
  en = list(
    title = "TaxoConsensus Hub 🔬",
    config = "Configuration",
    api_label = "Your Gemini API Key (Optional):",
    api_ph = "Leave blank to use public quota...",
    api_help = "👉 Get your free API Key here",
    lang = "Language / Idioma",
    filter = "Filter by Status:",
    custom_search = "Query specific species:",
    btn_custom = "🔍 Analyze Species",
    tot_reg = "Total Records",
    conf_mean = "Avg Confidence",
    synonyms = "Synonyms",
    ai_title = "AI Assistant (Results)",
    table_title = "Vector Explorer",
    empty_ai = "Select a species from the table or search manually.",
    analyzing = "Analyzing...",
    error_empty_search = "Please enter a taxon name first."
  )
)

# --- 3. DATASET DE EJEMPLO ---
# NOTA: Asegúrate de subir el archivo CSV junto con app.R a shinyapps.io
taxonomy_data <- rio::import("AI_Assistant_map_TaxoConsensus_Pro_app.csv") 

PROJECT_ID <- "PINV01-528"

# --- 4. FUNCIÓN LLAMADA GEMINI ---
call_gemini <- function(prompt, system_prompt, user_key = "") {
  
  # Lógica Híbrida: Prioriza la clave del usuario. Si está vacía, busca la del servidor.
  api_key <- user_key
  if (is.null(api_key) || trimws(api_key) == "") {
    api_key <- Sys.getenv("GEMINI_API_KEY")
  }
  
  if (api_key == "") stop("API Key no configurada. Por favor ingrese su clave o verifique el servidor.")
  
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-lite:generateContent?key=", api_key)
  
  # Truco para crear un objeto JSON vacío "{}" requerido por la API de Google Search
  empty_obj <- setNames(list(), character(0))
  
  body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    systemInstruction = list(parts = list(list(text = system_prompt))),
    
    # ALTERNATIVA 2: Activación de Grounding (Búsqueda en Google)
    tools = list(list(google_search = empty_obj)), 
    
    # ALTERNATIVA 3: Temperatura 0.0 (100% determinista para evitar alucinaciones)
    generationConfig = list(temperature = 0.0) 
  )
  
  res <- POST(url, body = body, encode = "json")
  content <- content(res, "parsed")
  if (!is.null(content$error)) stop(content$error$message)
  return(content$candidates[[1]]$content$parts[[1]]$text)
}

# --- 5. INTERFAZ DE USUARIO (Envuelto en secure_app) ---
ui <- secure_app(
  uiOutput("dynamic_ui"), 
  language = "es" 
)

# --- 6. LÓGICA DEL SERVIDOR ---
server <- function(input, output, session) {
  
  # Autenticación
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # Estado reactivo para el idioma
  current_lang <- reactiveVal("es")
  ai_response <- reactiveVal("")
  
  # Observar cambio de idioma
  observeEvent(input$lang_selector, {
    current_lang(input$lang_selector)
    if (ai_response() == "") {
      ai_response(paste0("<div style='text-align:center; padding: 20px; color: #6c757d;'>", i18n[[input$lang_selector]]$empty_ai, "</div>"))
    }
  })
  
  # Generación Dinámica de la Interfaz
  output$dynamic_ui <- renderUI({
    t <- i18n[[current_lang()]]
    
    page_sidebar(
      title = t$title,
      theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#4f46e5"),
      
      sidebar = sidebar(
        title = t$config,
        
        # NUEVO: Campo para la API Key personalizada y botón de ayuda
        passwordInput("user_api_key", t$api_label, placeholder = t$api_ph),
        helpText(a(t$api_help, href="https://aistudio.google.com/app/apikey", target="_blank")),
        hr(),
        
        selectInput("lang_selector", t$lang, choices = c("Español" = "es", "English" = "en"), selected = current_lang()),
        hr(),
        selectInput("status_filter", t$filter, choices = c("ALL", "ACCEPTED", "SYNONYM")),
        hr(),
        
        # Campo de búsqueda libre
        textInput("custom_taxon", t$custom_search, placeholder = "Ej: Lutzomyia longipalpis"),
        actionButton("btn_custom", t$btn_custom, class = "btn-secondary w-100"),
        
        p(class = "text-muted mt-4", style = "font-size: 0.8em;", paste("Usuario logueado:", res_auth$user))
      ),
      
      layout_columns(
        fill = FALSE,
        value_box(title = t$tot_reg, value = nrow(taxonomy_data), showcase = bsicons::bs_icon("list-ul")),
        # Evitamos errores de renderizado si las columnas no existen en tu CSV original
        value_box(title = t$conf_mean, value = if("conf" %in% names(taxonomy_data)) paste0(round(mean(taxonomy_data$conf, na.rm=TRUE), 1), "%") else "N/A", showcase = bsicons::bs_icon("check-circle")),
        value_box(title = "Sinónimos / Synonyms", value = if("status" %in% names(taxonomy_data)) sum(taxonomy_data$status == "SYNONYM", na.rm = TRUE) else "N/A", showcase = bsicons::bs_icon("exclamation-triangle"))
      ),
      
      card(full_screen = TRUE, card_header(t$ai_title), uiOutput("ai_result_ui")),
      card(full_screen = TRUE, card_header(t$table_title), DTOutput("vector_table"))
    )
  })
  
  # Filtro de Tabla
  filtered_data <- reactive({
    if (is.null(input$status_filter) || input$status_filter == "ALL") {
      taxonomy_data 
    } else if ("status" %in% names(taxonomy_data)) {
      taxonomy_data %>% filter(status == input$status_filter)
    } else {
      taxonomy_data # Fallback si el CSV cargado no tiene la columna 'status'
    }
  })
  
  output$vector_table <- renderDT({
    # Ajustar nombres de columnas si cambian al leer el CSV
    cols <- if(ncol(filtered_data()) >= 5) c("Disease", "Species (Clean)", "Match GBIF", "Confidence (%)", "Status") else names(filtered_data())
    datatable(filtered_data(), selection = "single", options = list(pageLength = 5, dom = 'ftip', scrollX = TRUE),
              rownames = FALSE, colnames = cols) %>%
      formatStyle(if("conf" %in% names(filtered_data())) 'conf' else 1, backgroundColor = styleInterval(94, c('#fef3c7', '#d1fae5')))
  })
  
  # Función auxiliar para analizar especies
  analyze_species <- function(species_name) {
    t <- i18n[[current_lang()]]
    
    # LOG EN SQLITE
    dbExecute(con, "INSERT INTO api_logs (user, timestamp, action, target) VALUES (?, ?, ?, ?)", 
              params = list(res_auth$user, as.character(Sys.time()), "Especie", species_name))
    
    ai_response(paste0("<div style='text-align:center;'><div class='spinner-border text-primary'></div><p>", t$analyzing, "</p></div>"))
    
    # NUEVO PROMPT: Formato Taxonómico Estricto + Cero Alucinaciones + APA sin DOI
    sys_prompt <- paste0(
      "Eres un experto entomólogo riguroso del proyecto ", PROJECT_ID, ". ",
      "Tu objetivo es CERO ALUCINACIONES. Basate exclusivamente en hechos científicos comprobables. ",
      "Estructura tu respuesta estrictamente en estas 4 secciones: 1. Correct Name, 2. Justification, 3. Medical Importance, 4. References. ",
      "REGLAS ESTRICTAS: ",
      "- Para '1. Correct Name', debes usar ESTRICTAMENTE el formato taxonómico completo: Género (Subgénero) especie Autor, Año. (Ejemplo: Anopheles (Nyssorhynchus) albitarsis Lynch Arribálzaga, 1878). Si el subgénero no existe, omite los paréntesis. ",
      "- Las referencias deben estar en formato APA puro, pero ESTRICTAMENTE SIN DOIs ni URLs (para evitar la generación de enlaces falsos). ",
      "- ESTRICTAMENTE PROHIBIDO inventar o incluir taxonIDs o LSIDs. ",
      "- OBLIGATORIO: Al final de tu documento, debes agregar exactamente esta nota en cursiva: ",
      "'⚠️ *Nota: Este recurso fue generado por un asistente de Inteligencia Artificial para el proyecto ", PROJECT_ID, " y puede contener errores. Se recomienda la validación con fuentes primarias.*' ",
      "Idioma de la respuesta: ", ifelse(current_lang()=="es", "Español", "English"), ". Formato Markdown."
    )
    
    tryCatch({
      # Pasamos el user_key capturado de la interfaz de usuario
      result <- call_gemini(paste("Resuelve:", species_name), sys_prompt, user_key = input$user_api_key)
      ai_response(markdown::markdownToHTML(text = result, fragment.only = TRUE))
    }, error = function(e) ai_response(paste("<div style='color:red;'><b>Error:</b>", e$message, "</div>")))
  }
  
  # Acción: Seleccionar Fila de la Tabla
  observeEvent(input$vector_table_rows_selected, {
    req(input$vector_table_rows_selected)
    
    selected_row <- filtered_data()[input$vector_table_rows_selected, ]
    # Detecta la columna que tenga el nombre limpio (ajustar si el CSV tiene otro nombre)
    species <- if("clean" %in% names(selected_row)) selected_row$clean else selected_row[[1]] 
    
    analyze_species(species)
  })
  
  # Acción: Botón Búsqueda Libre
  observeEvent(input$btn_custom, {
    t <- i18n[[current_lang()]]
    custom_species <- trimws(input$custom_taxon)
    
    if (custom_species == "") {
      showNotification(t$error_empty_search, type = "warning")
      return()
    }
    
    analyze_species(custom_species)
  })
  
  output$ai_result_ui <- renderUI({ HTML(ai_response()) })
}

# Cerrar conexión a BD al apagar la app
onStop(function() { dbDisconnect(con) })

shinyApp(ui, server)