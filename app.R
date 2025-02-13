# Cargar las librerías necesarias
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(lubridate)
library(forecast)  # Librería para forecasting
library(tidyr)

# Simulación de datos para Dashboard 2: Portafolio y Rentabilidad
set.seed(123)

risk_data <- data.frame(
  Date = seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-01"), by = "month"),
  RiskIndex = round(runif(12, 20, 80), 0)
)
risk_dimensions <- c("Climático", "Operativo", "Financiero")
# Simular datos de riesgo para cada dimensión por mes (heatmap)
risk_heatmap_data <- expand.grid(
  Month = format(risk_data$Date, "%b %Y"),
  Dimension = risk_dimensions
)
risk_heatmap_data$RiskValue <- round(runif(nrow(risk_heatmap_data), 20, 80), 0)

# Simular datos para el Radar Chart: riesgo de un mes (elegiremos el último mes)
current_risk <- risk_heatmap_data %>% 
  filter(Month == tail(format(risk_data$Date, "%b %Y"),1)) %>% 
  arrange(Dimension)
radar_values <- current_risk$RiskValue

dates <- seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-31"), by = "month")
portfolio_data <- data.frame(
  Date = dates,
  Primas = round(runif(length(dates), 100000, 500000), 0),
  Siniestralidad = round(runif(length(dates), 0.1, 0.5), 2)
)

# Calcular monto de pérdidas y ratio de pérdidas
portfolio_data <- portfolio_data %>%
  mutate(
    LossAmount = Primas * Siniestralidad,
    LossRatio = round(LossAmount / Primas, 2)  # Equivalente a Siniestralidad
  )

# Generar predicciones para Primas utilizando una serie de tiempo
ts_primas <- ts(portfolio_data$Primas, frequency = 12, start = c(2024, 1))
forecast_primas <- forecast(ts_primas, h = 2)
forecast_primas_values <- as.numeric(forecast_primas$mean)

# Generar predicciones para Siniestralidad (Índice de Siniestralidad)
ts_siniestralidad <- ts(portfolio_data$Siniestralidad, frequency = 12, start = c(2024, 1))
forecast_siniestralidad <- forecast(ts_siniestralidad, h = 2)
forecast_siniestralidad_values <- as.numeric(forecast_siniestralidad$mean)

# Generar predicciones para Ratio de Pérdidas
ts_lossRatio <- ts(portfolio_data$LossRatio, frequency = 12, start = c(2024, 1))
forecast_lossRatio <- forecast(ts_lossRatio, h = 2)
forecast_lossRatio_values <- as.numeric(forecast_lossRatio$mean)

# Datos para Dashboard 3: Reclamaciones y Siniestralidad
claim_status <- data.frame(
  name = c("En Proceso", "Resueltas", "Rechazadas"),
  y = c(sample(50:100, 1), sample(100:150, 1), sample(10:30, 1))
)

siniestralidad_data <- data.frame(
  Category = c("Daños menores", "Daños mayores", "Otros"),
  Count = sample(30:80, 3)
)

# Dashboard 4: Cumplimiento Normativo y Reporte Regulatorio
compliance_data <- data.frame(
  Date = seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-01"), by = "month"),
  ComplianceScore = round(runif(12, 70, 100), 0)
)
compliance_time <- data.frame(
  Month = format(compliance_data$Date, "%b %Y"),
  Cumple = round(runif(12, 70, 120), 0),
  NoCumple = round(runif(12, 10, 30), 0),
  Pendiente = round(runif(12, 20, 40), 0)
)
# Para el Bullet Chart: uso del dato más reciente
actual_compliance <- tail(compliance_data$ComplianceScore, 1)
target_compliance <- 90


# Interfaz de usuario con menú para cada dashboard
ui <- dashboardPage(
  dashboardHeader(title = "ECUARE Dashboards"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portafolio y Rentabilidad", tabName = "portafolio", icon = icon("chart-line")),
      menuItem("Riesgos y Eventos Naturales", tabName = "riesgos", icon = icon("exclamation-circle")),
      menuItem("Reclamaciones y Siniestralidad", tabName = "reclamaciones", icon = icon("exclamation-triangle")),
      menuItem("Cumplimiento Normativo", tabName = "cumplimiento", icon = icon("gavel"))
    )
  ),
  dashboardBody(
    tags$head(
      # CSS personalizado para mejorar el aspecto (UX/UI)
      tags$style(HTML("
        .skin-blue .main-header .logo { background-color: #1f71ad; }
        .skin-blue .main-header .navbar { background-color: #1f71ad; }
        .content-wrapper, .right-side { background-color: #ecf0f5; }
      ")),
      # JavaScript personalizado: por ejemplo, muestra una alerta cuando se hace clic en los gráficos
      tags$script(HTML("
        $(document).on('shiny:connected', function(event) {
          console.log('Shiny app connected.');
        });
        function chartClicked() {
          alert('¡Has interactuado con el gráfico!');
        }
      "))
    ),
    tabItems(
      
      # Dashboard 1: Riesgos y Eventos Naturales
      tabItem(tabName = "riesgos",
              fluidRow(
                box(title = "Radar de Riesgo (Último Mes)", status = "primary", solidHeader = TRUE, width = 6,
                    highchartOutput("riskRadar", height = "350px")
                ),
                box(title = "Heatmap de Riesgo por Dimensión", status = "primary", solidHeader = TRUE, width = 6,
                    highchartOutput("riskHeatmap", height = "350px")
                )
              )
      ),
      # Dashboard 2: Portafolio y Rentabilidad
      tabItem(tabName = "portafolio",
              fluidRow(
                box(title = "Primas Cobradas (Mensual)", status = "primary", solidHeader = TRUE, width = 12,
                    highchartOutput("lineChart", height = "300px")
                )
              ),
              fluidRow(
                box(title = "Índice de Siniestralidad Mensual", status = "primary", solidHeader = TRUE, width = 12,
                    highchartOutput("barChart", height = "300px")
                )
              ),
              fluidRow(
                box(title = "Ratio de Pérdidas Mensuales", status = "primary", solidHeader = TRUE, width = 12,
                    highchartOutput("lossRatioChart", height = "300px")
                )
              )
      ),
      # Dashboard 3: Reclamaciones y Siniestralidad
      tabItem(tabName = "reclamaciones",
              fluidRow(
                box(title = "Distribución de Reclamaciones", status = "warning", solidHeader = TRUE, width = 6,
                    highchartOutput("pieChart", height = "300px")
                ),
                box(title = "Categorías de Siniestralidad", status = "warning", solidHeader = TRUE, width = 6,
                    highchartOutput("columnChart", height = "300px")
                )
              )
      ),
      # Dashboard 4: Cumplimiento Normativo
      tabItem(tabName = "cumplimiento",
              fluidRow(
                box(title = "Estado de Cumplimiento Mensual (Apilado)", status = "success", solidHeader = TRUE, width = 12,
                    highchartOutput("stackedCompliance", height = "300px")
                )
              ),
              fluidRow(
                box(title = "Cumplimiento Actual vs Objetivo", status = "success", solidHeader = TRUE, width = 12,
                    highchartOutput("complianceBullet", height = "200px")
                )
              )
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  ## Dashboard 1: Riesgos y Eventos Naturales
  
  # Radar Chart: mostrar índices de riesgo en 3 dimensiones para el último mes
  output$riskRadar <- renderHighchart({
    highchart() %>%
      hc_chart(polar = TRUE, type = "line") %>%
      hc_title(text = "Radar de Riesgo - Último Mes") %>%
      hc_xAxis(categories = risk_dimensions,
               tickmarkPlacement = "on",
               lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = "polygon",
               min = 0, max = 100,
               title = list(text = "Índice de Riesgo")) %>%
      hc_series(
        list(
          name = tail(format(risk_data$Date, "%b %Y"),1),
          data = radar_values,
          pointPlacement = "on"
        )
      ) %>%
      hc_tooltip(shared = TRUE) %>%
      hc_chart(events = list(click = JS("function() { chartClicked(); }")))
  })
  
  # Heatmap: riesgo por dimensión a lo largo de los meses
  output$riskHeatmap <- renderHighchart({
    # Transformar los datos para el heatmap: filas: Dimension, columnas: Month
    data_matrix <- risk_heatmap_data %>%
      pivot_wider(names_from = Month, values_from = RiskValue) %>%
      as.data.frame()
    # Eliminar la columna de dimensión para obtener un vector con cada celda
    categories <- colnames(data_matrix)[-1]
    data_long <- risk_heatmap_data %>%
      mutate(x = match(Month, categories) - 1,
             y = match(Dimension, risk_dimensions) - 1) %>%
      select(x, y, value = RiskValue) %>%
      as.data.frame()
    
    highchart() %>%
      hc_chart(type = "heatmap") %>%
      hc_title(text = "Heatmap de Riesgo por Dimensión") %>%
      hc_xAxis(categories = categories, title = list(text = "Mes")) %>%
      hc_yAxis(categories = risk_dimensions, title = list(text = "Dimensión")) %>%
      hc_colorAxis(min = 0, minColor = "#FFFFFF", maxColor = "#DF5353") %>%
      hc_series(
        list(
          name = "Riesgo",
          data = list_parse(data_long),
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_tooltip(pointFormat = "Mes: {point.xCategory}<br>Dimensión: {point.yCategory}<br>Riesgo: {point.value}") %>%
      hc_chart(events = list(click = JS("function() { chartClicked(); }")))
  })
  # Gráfico de línea: Primas cobradas con predicción
  output$lineChart <- renderHighchart({
    # Generar fechas para la parte histórica y de forecast
    forecast_dates <- seq.Date(from = max(portfolio_data$Date) %m+% months(1), by = "month", length.out = 2)
    all_dates <- c(portfolio_data$Date, forecast_dates)
    
    highchart() %>%
      hc_title(text = "Primas Cobradas Mensuales") %>%
      hc_xAxis(categories = format(all_dates, "%b %Y")) %>%
      # Serie histórica
      hc_add_series(name = "Primas Históricas", data = portfolio_data$Primas, type = "line") %>%
      # Serie de forecast (alineando con NA en la parte histórica)
      hc_add_series(name = "Predicción", 
                    data = c(rep(NA, nrow(portfolio_data)), forecast_primas_values), 
                    type = "line", dashStyle = "Dash") %>%
      hc_tooltip(shared = TRUE) %>%
      hc_chart(events = list(
        click = JS("function() { chartClicked(); }")
      ))
  })
  
  # Gráfico de columnas: Índice de Siniestralidad con predicción
  output$barChart <- renderHighchart({
    forecast_dates <- seq.Date(from = max(portfolio_data$Date) %m+% months(1), by = "month", length.out = 2)
    all_dates <- c(portfolio_data$Date, forecast_dates)
    
    highchart() %>%
      hc_title(text = "Índice de Siniestralidad Mensual") %>%
      hc_xAxis(categories = format(all_dates, "%b %Y")) %>%
      # Serie histórica en columnas
      hc_add_series(name = "Siniestralidad Histórica", data = portfolio_data$Siniestralidad, type = "column") %>%
      # Serie de forecast sobreimpresa como línea discontinua
      hc_add_series(name = "Predicción", 
                    data = c(rep(NA, nrow(portfolio_data)), forecast_siniestralidad_values), 
                    type = "line", dashStyle = "Dash") %>%
      hc_tooltip(shared = TRUE) %>%
      hc_chart(events = list(
        click = JS("function() { chartClicked(); }")
      ))
  })
  
  # Gráfico de línea: Ratio de Pérdidas con predicción
  output$lossRatioChart <- renderHighchart({
    forecast_dates <- seq.Date(from = max(portfolio_data$Date) %m+% months(1), by = "month", length.out = 2)
    all_dates <- c(portfolio_data$Date, forecast_dates)
    
    highchart() %>%
      hc_title(text = "Ratio de Pérdidas Mensuales") %>%
      hc_xAxis(categories = format(all_dates, "%b %Y")) %>%
      hc_add_series(name = "Ratio Históricos", data = portfolio_data$LossRatio, type = "line") %>%
      hc_add_series(name = "Predicción", 
                    data = c(rep(NA, nrow(portfolio_data)), forecast_lossRatio_values), 
                    type = "line", dashStyle = "Dash") %>%
      hc_tooltip(shared = TRUE) %>%
      hc_chart(events = list(
        click = JS("function() { chartClicked(); }")
      ))
  })
  
  # Gráfico de pastel: Distribución de Reclamaciones
  output$pieChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Estado de Reclamaciones") %>%
      hc_series(
        list(
          name = "Reclamaciones",
          data = list_parse2(claim_status)
        )
      ) %>%
      hc_tooltip(pointFormat = "{point.name}: <b>{point.y}</b> reclamaciones") %>%
      hc_plotOptions(pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y}")
      ))
  })
  
  # Gráfico de columnas: Categorías de Siniestralidad
  output$columnChart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Categorías de Siniestralidad") %>%
      hc_xAxis(categories = siniestralidad_data$Category) %>%
      hc_add_series(name = "Siniestralidad", data = siniestralidad_data$Count) %>%
      hc_tooltip(pointFormat = "Casos: <b>{point.y}</b>") %>%
      hc_plotOptions(column = list(
        cursor = "pointer",
        events = list(
          click = JS("function() { chartClicked(); }")
        )
      ))
  })
  
  ## Dashboard 4: Cumplimiento Normativo
  
  output$stackedCompliance <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Estado de Cumplimiento Mensual") %>%
      hc_xAxis(categories = compliance_time$Month) %>%
      hc_yAxis(min = 0, title = list(text = "Número de Casos")) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(name = "Cumple", data = compliance_time$Cumple) %>%
      hc_add_series(name = "No Cumple", data = compliance_time$NoCumple) %>%
      hc_add_series(name = "Pendiente", data = compliance_time$Pendiente) %>%
      hc_tooltip(shared = TRUE)
  })
  
  output$complianceBullet <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bullet") %>%
      hc_add_dependency("modules/bullet.js") %>%  # Agrega el módulo de bullet
      hc_title(text = "Cumplimiento Actual vs Objetivo") %>%
      hc_yAxis(min = 0, max = 120, title = list(text = "Índice de Cumplimiento")) %>%
      hc_add_series(
        list(
          data = list(list(y = actual_compliance, target = target_compliance))
        )
      ) %>%
      hc_tooltip(pointFormat = "Actual: <b>{point.y}</b><br>Objetivo: <b>{point.target}</b>")
  })
  
}

# Ejecutar la aplicación
shinyApp(ui, server)
