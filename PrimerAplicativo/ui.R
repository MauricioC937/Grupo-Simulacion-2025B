library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel(fluidRow(
    column(3, tags$img(src="epn.png", width="70px", height="70px")),
    column(9, h1("Titulo del Aplicativo", style = "text-align:center; color:#132b60; 
                       padding:18px;font-size:1.1em; font-family: roman"))
  )),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargar archivo Excel", accept = ".xlsx"),
      uiOutput("sheet_ui"),
      actionButton("analizar", "Carga datos"),
      hr(),
      selectInput("colx", "Seleccione el color:", c("Rojo" = "red", "Verde" = "green", "Azul" = "blue",'Amarillo'='yellow')),
      hr(),
      downloadButton("descargar_pdf", "Informe PDF")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$style("h1 {color: #132b60; font-family: roman; font-size:1.7em}"),
      navbarPage("Análisis",
                 tabPanel("Datos",
                          h1("Registros de la base de datos"),
                          tableOutput("TablaVariable"),
                          hr(),
                          h1("Número de créditos otorgados por Provincia"),
                          tableOutput("CreditosXProv"),
                          hr(),
                          h1("Resumen de Activos, Crédito y Mora según Sexo y Número de Cargas"),
                          tableOutput("TablaSexoCargas"),
                          hr(),
                          h1("Análisis de Profesión"),
                          tableOutput("TablaProfesion"),
                          hr(),
                          h1("Tabla por rango de Edades"),
                          uiOutput("TablaEdades")
                 ),
                 tabPanel("Gráficos",
                          fluidRow(
                            column(12, h1("Gráfico de barras"),
                                   uiOutput("VarCualitativa"),
                                   plotOutput("Grafico01")
                            ),
                            column(12, h1("Gráfico Dispersión")),
                            column(6, uiOutput("VarCuantitativa")),
                            column(6, uiOutput("VarCuantitativa2")),
                            column(12, plotOutput("Grafico02")),
                            column(12, h1("Análisis Monto vs Cuota por Profesión")),
                            column(12, plotOutput("GraficoProfesion")),
                            column(12 ,h1("Análisis por grupo de edad")),
                            column(12, plotOutput("GraficoEdades1")),
                            column(12, plotOutput("GraficoEdades3")),
                            column(12, plotOutput("GraficoEdades4"))
                          )
                 ),
                 tabPanel("Informe")
      )
    )
  )
)


