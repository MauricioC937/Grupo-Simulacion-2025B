#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel(
      fluidRow(
        column(width=2),
        column(width=10,h1('Aplicativo Web'))
      )
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width=3,
            numericInput("num",
                        "Numero de variables:",
                        min = 1,
                        max = 500,
                        value = 20),
                        radioButtons('metodo',
                                     'Seleccione el método de generación;',
                                     c('Congruencial multiplicativo',
                                       'Congruencial mixto',
                                       'Cuadrados Medios',
                                       'Lehmer')),
            hr(),
            h3('Parámetros'),
            numericInput('x0','Semilla inicial',min=99,max=2^31-1,value=151),
            numericInput('a','Constante a:',min=1,max=2^31-1,value=31),
            numericInput('m','Constante m:',min=1,max=2^31-1,value=73),
            numericInput('c','Constante c:',min=1,max=2^31-1,value=97),
            hr(),
            actionButton('simular','Simular',icon = icon('circle-play'))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            navbarPage(icon('book'),
                       tabPanel("Numéros Aleatorios",
                                verbatimTextOutput('code'),
                                plotOutput('grafico'),
                                verbatimTextOutput('resumen'),
                                hr(),
                                conditionalPanel(condition = 'input.simular==1',
                                                 h3('Calculo de integrales'),
                                                 fluidRow(
                                                   column(4,textInput('func','Ingrese la función que desea evaluar')),
                                                   column(4,numericInput('linf','Límite inferior',min=0,max = 10,value = 0)),
                                                   column(4, numericInput('lsup','Límite superior',min=0,max = 10,value = 1))
                                                   ),
                                                 plotOutput('graficofun'),
                                                 h3('El resultado de la integral es:'),
                                                 verbatimTextOutput('integral')
                                                 )
                                ),
                       tabPanel("Variables aleatorias Discretas"),
                       tabPanel("Variables aleatorias Continuas"))
        )
    )
)
