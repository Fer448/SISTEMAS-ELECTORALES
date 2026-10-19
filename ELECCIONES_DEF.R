# =============================================================================
# Dashboard de resultados electorales - Comparador de sistemas de reparto
# Métodos: Hare, Saint Laguë, d'Hondt, Imperiali, Danés, Danés corregido
#
# Paquetes necesarios:
#   install.packages(c("shiny","shinydashboard","readxl","dplyr","tidyr",
#                      "DT","plotly","openxlsx"))
# Ejecutar con: shiny::runApp("app.R")
# =============================================================================

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(openxlsx)

METODOS <- c("Hare", "Saint Laguë", "d'Hondt", "Imperiali",
             "Danés", "Danés corregido")

# -----------------------------------------------------------------------------
# FUNCIONES DE ASIGNACIÓN
# -----------------------------------------------------------------------------

# Métodos de divisores (promedios más altos).
# divisor_fn(k) devuelve el k-ésimo divisor (k = 1, 2, ..., N).
# Desempate: mayor número de votos y, si persiste, orden de aparición.
alloc_divisores <- function(votos, N, divisor_fn) {
  n <- length(votos)
  if (n == 0 || N <= 0) return(integer(n))
  d  <- divisor_fn(seq_len(N))
  q  <- outer(votos, d, "/")                      # n x N
  df <- data.frame(p = rep(seq_len(n), times = N), q = as.vector(q))
  df$v <- votos[df$p]
  df <- df[order(-df$q, -df$v, df$p), ]
  tabulate(df$p[seq_len(N)], nbins = n)
}

# Métodos de cuota (resto mayor).
# extra = 0 -> cuota Hare  V/N
# extra = 2 -> cuota Imperiali V/(N+2); si se reparten más escaños de los
#              disponibles se aumenta el divisor (N+3, N+4, ...) hasta que
#              el reparto por cuota no exceda N.
alloc_resto <- function(votos, N, extra = 0) {
  n <- length(votos); V <- sum(votos)
  if (n == 0 || V == 0 || N <= 0) return(integer(n))
  divisor <- N + extra
  repeat {
    q   <- V / divisor
    esc <- floor(votos / q + 1e-9)
    # si se reparten más escaños de los disponibles, se pasa de N+2 a N+1
    # y luego a N (cuota Hare), que nunca excede N
    if (sum(esc) <= N || divisor <= N) break
    divisor <- divisor - 1
  }
  restos <- votos / q - esc
  falt   <- N - sum(esc)
  if (falt > 0) {
    ord <- order(-restos, -votos, seq_len(n))
    sel <- ord[seq_len(falt)]
    esc[sel] <- esc[sel] + 1
  }
  as.integer(esc)
}

calcular_todos <- function(votos, N, primer_divisor_danes_corr = 1.4) {
  list(
    "Hare"            = alloc_resto(votos, N, extra = 0),
    "Saint Laguë"     = alloc_divisores(votos, N, function(k) 2 * k - 1),
    "d'Hondt"         = alloc_divisores(votos, N, function(k) k),
    "Imperiali"       = alloc_resto(votos, N, extra = 2),
    "Danés"           = alloc_divisores(votos, N, function(k) 3 * k - 2),   # 1,4,7,10...
    "Danés corregido" = alloc_divisores(votos, N, function(k)
      ifelse(k == 1, primer_divisor_danes_corr, 3 * k - 2))                 # f,4,7,10...
  )
}

# Índice de desproporcionalidad de Gallagher
gallagher <- function(pct_votos, pct_escanos) {
  sqrt(0.5 * sum((pct_votos - pct_escanos)^2))
}

# -----------------------------------------------------------------------------
# INTERFAZ
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Simulador electoral"),
  dashboardSidebar(
    width = 300,
    br(),
    fileInput("archivo", "1. Archivo Excel (votos absolutos)",
              accept = c(".xlsx", ".xls"),
              buttonLabel = "Examinar...", placeholder = "Ningún archivo"),
    helpText(style = "padding: 0 15px; color:#b8c7ce;",
             "Formato: una columna con el nombre del partido y otra con los",
             "votos (números). Se usa la primera hoja."),
    numericInput("escanos", "2. Número de escaños", value = 10, min = 1, step = 1),
    numericInput("barrera", "3. Barrera electoral (% sobre total de votos)",
                 value = 3, min = 0, max = 100, step = 0.1),
    numericInput("div_danes", "Primer divisor del Danés corregido",
                 value = 1.4, min = 0.1, step = 0.1),
    hr(),
    downloadButton("descarga", "Descargar resultados (.xlsx)",
                   style = "margin-left:15px; color:#333;")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("vb_votos", width = 3),
      valueBoxOutput("vb_partidos", width = 3),
      valueBoxOutput("vb_validos", width = 3),
      valueBoxOutput("vb_escanos", width = 3)
    ),
    fluidRow(
      box(title = "Escaños por partido y método", status = "primary",
          solidHeader = TRUE, width = 12,
          DTOutput("tabla_resultados"))
    ),
    fluidRow(
      box(title = "Comparación gráfica de escaños", status = "primary",
          solidHeader = TRUE, width = 8,
          plotlyOutput("grafico", height = "420px")),
      box(title = "Desproporcionalidad (índice de Gallagher)", status = "warning",
          solidHeader = TRUE, width = 4,
          plotlyOutput("grafico_gall", height = "300px"),
          helpText("Menor valor = reparto más proporcional a los votos."))
    ),
    fluidRow(
      box(title = "Votos y barrera", status = "info", solidHeader = TRUE,
          width = 6, DTOutput("tabla_votos")),
      box(title = "Notas metodológicas", status = "info", solidHeader = TRUE,
          width = 6, collapsible = TRUE,
          tags$ul(
            tags$li(strong("Hare:"), " cuota V/N y resto mayor."),
            tags$li(strong("Saint Laguë:"), " divisores 1, 3, 5, 7..."),
            tags$li(strong("d'Hondt:"), " divisores 1, 2, 3, 4..."),
            tags$li(strong("Imperiali:"), " cuota V/(N+2) y resto mayor; si se",
                    " exceden los escaños, se aumenta el divisor hasta ajustar."),
            tags$li(strong("Danés:"), " divisores 1, 4, 7, 10..."),
            tags$li(strong("Danés corregido:"), " divisores f, 4, 7, 10..., donde",
                    " f es el primer divisor editable en el panel lateral",
                    " (por defecto 1,4)."),
            tags$li("La barrera se aplica sobre el total de votos del archivo;",
                    " los partidos que no la alcanzan quedan excluidos del reparto",
                    " y V se calcula solo con los partidos que la superan."),
            tags$li("Empates: se resuelven a favor del partido con más votos.")
          ))
    )
  )
)

# -----------------------------------------------------------------------------
# SERVIDOR
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # Lectura del Excel: detecta primera columna de texto (partido)
  # y primera columna numérica (votos)
  datos <- reactive({
    req(input$archivo)
    raw <- tryCatch(read_excel(input$archivo$datapath),
                    error = function(e) NULL)
    validate(need(!is.null(raw) && nrow(raw) > 0,
                  "No se pudo leer el archivo Excel."))
    col_num <- which(sapply(raw, is.numeric))
    col_txt <- which(!sapply(raw, is.numeric))
    validate(need(length(col_num) >= 1,
                  "El archivo debe tener una columna numérica con los votos."))
    nombres <- if (length(col_txt) >= 1) as.character(raw[[col_txt[1]]])
               else paste("Partido", seq_len(nrow(raw)))
    df <- data.frame(Partido = nombres, Votos = as.numeric(raw[[col_num[1]]]),
                     stringsAsFactors = FALSE)
    df <- df[!is.na(df$Partido) & !is.na(df$Votos) & df$Votos >= 0, ]
    validate(need(nrow(df) > 0 && sum(df$Votos) > 0,
                  "No hay votos válidos en el archivo."))
    df
  })

  resultados <- reactive({
    df <- datos()
    N  <- input$escanos
    validate(need(!is.na(N) && N >= 1, "Indique un número de escaños válido."),
             need(!is.na(input$barrera) && input$barrera >= 0 && input$barrera <= 100,
                  "La barrera debe estar entre 0 y 100."))
    N <- as.integer(N)
    tot <- sum(df$Votos)
    df$Pct  <- df$Votos / tot * 100
    df$Pasa <- df$Pct >= input$barrera

    elegibles <- which(df$Pasa)
    res <- setNames(lapply(METODOS, function(m) integer(nrow(df))), METODOS)
    if (length(elegibles) > 0) {
      asig <- calcular_todos(df$Votos[elegibles], N, input$div_danes)
      for (m in METODOS) res[[m]][elegibles] <- asig[[m]]
    }
    out <- cbind(df, as.data.frame(res, check.names = FALSE))
    list(tabla = out, N = N, total = tot, n_elegibles = length(elegibles),
         votos_elegibles = sum(df$Votos[df$Pasa]))
  })

  # Indicadores
  output$vb_votos <- renderValueBox({
    valueBox(format(resultados()$total, big.mark = ".", decimal.mark = ","),
             "Votos totales", icon = icon("users"), color = "blue")
  })
  output$vb_partidos <- renderValueBox({
    r <- resultados()
    valueBox(paste(r$n_elegibles, "de", nrow(r$tabla)),
             "Partidos que superan la barrera", icon = icon("flag"), color = "green")
  })
  output$vb_validos <- renderValueBox({
    r <- resultados()
    valueBox(sprintf("%.1f%%", r$votos_elegibles / r$total * 100),
             "Votos que entran en el reparto", icon = icon("percent"),
             color = "yellow")
  })
  output$vb_escanos <- renderValueBox({
    valueBox(resultados()$N, "Escaños a repartir", icon = icon("landmark"),
             color = "purple")
  })

  # Tabla comparativa principal
  tabla_comp <- reactive({
    t <- resultados()$tabla
    tot <- data.frame(Partido = "TOTAL", Votos = sum(t$Votos),
                      Pct = sum(t$Pct), Pasa = NA)
    for (m in METODOS) tot[[m]] <- sum(t[[m]])
    bind_rows(t, tot)
  })

  output$tabla_resultados <- renderDT({
    t <- tabla_comp()
    t$Pasa <- ifelse(is.na(t$Pasa), "", ifelse(t$Pasa, "Sí", "No"))
    names(t)[names(t) == "Pct"]  <- "% votos"
    names(t)[names(t) == "Pasa"] <- "Supera barrera"
    datatable(t, rownames = FALSE, class = "stripe hover compact",
              options = list(pageLength = 25, dom = "t", ordering = FALSE,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = 2:(ncol(t) - 1))))) %>%
      formatRound("% votos", 2) %>%
      formatRound("Votos", 0, mark = ".") %>%
      formatStyle("Partido", target = "row",
                  fontWeight = styleEqual("TOTAL", "bold"),
                  backgroundColor = styleEqual("TOTAL", "#ecf0f5")) %>%
      formatStyle(METODOS, fontWeight = "bold")
  })

  # Gráfico de barras agrupadas
  output$grafico <- renderPlotly({
    t <- resultados()$tabla
    t <- t[t$Pasa, ]
    validate(need(nrow(t) > 0, "Ningún partido supera la barrera."))
    largo <- t %>%
      select(Partido, all_of(METODOS)) %>%
      pivot_longer(-Partido, names_to = "Método", values_to = "Escaños")
    largo$Método  <- factor(largo$Método, levels = METODOS)
    largo$Partido <- factor(largo$Partido, levels = t$Partido[order(-t$Votos)])
    plot_ly(largo, x = ~Partido, y = ~Escaños, color = ~Método, type = "bar",
            hovertemplate = "%{x}<br>%{fullData.name}: %{y} escaños<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = ""), yaxis = list(title = "Escaños"),
             legend = list(orientation = "h", y = -0.2))
  })

  # Índice de Gallagher por método
  output$grafico_gall <- renderPlotly({
    r <- resultados(); t <- r$tabla
    g <- sapply(METODOS, function(m) gallagher(t$Pct, t[[m]] / r$N * 100))
    d <- data.frame(Método = factor(METODOS, levels = METODOS),
                    Gallagher = round(g, 2))
    plot_ly(d, x = ~Método, y = ~Gallagher, type = "bar",
            color = ~Método, showlegend = FALSE,
            text = ~Gallagher, textposition = "outside",
            hovertemplate = "%{x}: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickangle = -30),
             yaxis = list(title = "Índice"))
  })

  # Tabla de votos y barrera
  output$tabla_votos <- renderDT({
    t <- resultados()$tabla[, c("Partido", "Votos", "Pct", "Pasa")]
    t <- t[order(-t$Votos), ]
    t$Pasa <- ifelse(t$Pasa, "Sí", "No")
    names(t) <- c("Partido", "Votos", "% votos", "Supera barrera")
    datatable(t, rownames = FALSE, options = list(pageLength = 10, dom = "tp")) %>%
      formatRound("% votos", 2) %>%
      formatRound("Votos", 0, mark = ".") %>%
      formatStyle("Supera barrera",
                  color = styleEqual(c("Sí", "No"), c("#00a65a", "#dd4b39")),
                  fontWeight = "bold")
  })

  # Descarga a Excel
  output$descarga <- downloadHandler(
    filename = function() paste0("resultados_electorales_", Sys.Date(), ".xlsx"),
    function(file) {
      r <- resultados(); t <- r$tabla
      gal <- data.frame(Método = METODOS,
                        Gallagher = sapply(METODOS, function(m)
                          gallagher(t$Pct, t[[m]] / r$N * 100)))
      write.xlsx(list("Resultados" = tabla_comp(), "Gallagher" = gal),
                 file = file)
    }
  )
}

shinyApp(ui, server)
