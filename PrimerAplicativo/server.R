library(shiny)
library(tidyverse)
library(readxl)
library(kableExtra)
library(formattable)
library(quarto)
library(lubridate)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$VarCualitativa <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "character") %>% pull(Variable)
    selectInput("VarCual", "Seleccione la variable:", choices = vars)
  })
  
  output$VarCuantitativa <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "numeric") %>% pull(Variable)
    selectInput("VarCuan", "Seleccione la variable:", choices = vars)
  })
  output$VarCuantitativa2 <- renderUI({
    vars <- data.frame(Variable = names(datos()), 
                       Tipo = unname(unlist(sapply(datos(), function(j){class(j)[1]})))) %>% 
      dplyr::filter(Tipo == "numeric") %>% pull(Variable)
    selectInput("VarCuan2", "Seleccione la variable:", choices = vars)
  })
  
  output$Grafico01 <- renderPlot({
    t01 <- datos() %>% select(input$VarCual)
    colnames(t01) <- "Variable"
    
    t01 <- t01 %>% group_by(Variable) %>% summarise(Registros = n()) %>% 
      ggplot(aes(x=Variable, y=Registros)) + geom_col(fill = input$colx)
    return(t01)
  })
  
  output$Grafico02 <- renderPlot({
    t02 <- datos() %>% select(input$VarCuan, input$VarCuan2)
    colnames(t02) <- c("Variable1", "Variable2")
    
    t02 <- t02 %>% ggplot(aes(x=Variable1, y=Variable2)) + geom_point(fill = input$colx)
    return(t02)
  })
  
  output$TablaVariable <- function(){
    res01 <- datos() %>% select(IDENTIFICACION, FECHAADJUDICACION, ESTADO_CIVIL, MONTO_OTORGADO)
    res01 %>% kable() %>% kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff",) %>% 
      scroll_box(width = "700px", height = "300px")
  }
  
  output$CreditosXProv <- function(){
    res02 <- datos() %>% group_by(PROVINCIA_DOMICILIO) %>% summarise(Registros = n()) %>% 
      mutate(PORCENTAJE = percent(Registros/sum(Registros))) %>% arrange(desc(Registros))
    colnames(res02) <- c("PROVINCIA", "CREDITOS", "PORCENTAJE")
    res02$CREDITOS = cell_spec(res02$CREDITOS, color = ifelse(res02$CREDITOS <= 100, "red", "blue"))
    res02$PORCENTAJE <- color_bar("lightgreen")(res02$PORCENTAJE)
    
    tab02 <- res02 %>% kable("html", escape = F, booktabs = TRUE) %>% kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff")
    HTML(tab02)
  }

output$TablaSexoCargas <- function(){
    res03 <- datos() %>%
      filter(!is.na(TOTALACTIVOS)) %>%
      group_by(SEXO, NUMERO_CARGAS) %>% 
      summarise(
        Min_TOTALACTIVOS = min(TOTALACTIVOS, na.rm = TRUE),
        Max_TOTALACTIVOS = max(TOTALACTIVOS, na.rm = TRUE),
        Promedio_MontoCredito = round(mean(MONTO_OTORGADO, na.rm = TRUE),2),
        Promedio_DiasMora = round(mean(DIAS_MORA_CON_TRAD, na.rm = TRUE),2)
      ) %>%
      mutate(
        Min_TOTALACTIVOS = paste('$', round(Min_TOTALACTIVOS, 2)),
        Max_TOTALACTIVOS = paste('$', round(Max_TOTALACTIVOS, 2)),
        Promedio_MontoCredito = paste('$', round(Promedio_MontoCredito, 2)),
        Porcentaje_Mora = paste0(round(Promedio_DiasMora * 100, 2), "%")
      ) %>%
      select(SEXO, NUMERO_CARGAS, Min_TOTALACTIVOS, Max_TOTALACTIVOS, 
         Promedio_MontoCredito, Porcentaje_Mora)
    colnames(res03) <- c('Sexo', 'Número Cargas', 'Min Total Activos','Max Total Activos', 'Crédito Promedio', 'Mora Promedio')
    tab03 <- res03 %>% kable("html", booktabs = TRUE) %>% kable_styling(font_size = 11, full_width = FALSE) %>%
      row_spec(0, background = "#132b60", color = "#ffffff")
    HTML(tab03)
    }
  # Gráfica de la información
  output$GraficoSexoCargas <- renderPlot({
    datos() %>%
      filter(!is.na(TOTALACTIVOS)) %>%
      group_by(SEXO, NUMERO_CARGAS) %>% 
      summarise(
        Promedio_MontoCredito = mean(MONTO_OTORGADO, na.rm = TRUE),
        Promedio_DiasMora = mean(DIAS_MORA_CON_TRAD, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = factor(NUMERO_CARGAS),
                 y = Promedio_MontoCredito,
                 fill = SEXO)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(
        title = "Crédito Promedio por Sexo y Número de Cargas",
        x = "Número de Cargas",
        y = "Crédito Promedio ($)",
        fill = "Sexo"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )
  })

  #TABLA DE PROFESIÓN
  output$TablaProfesion <- function(){
    req(datos())
    
      datos_df <- datos()
      #Buscar columna de profesión
      profesion_col <- NULL
      for (col in names(datos_df)) {
        if (grepl("profesion", tolower(col))) {
          profesion_col <- col
          break
        }
      }
      
      if (is.null(profesion_col)) {
        stop("No se encontró columna de profesión")
      }
      
      cat("Usando columna de profesión:", profesion_col, "\n")
      
      #Tbla simple de conteo por profesión
      tabla <- datos_df %>%
        rename(Profesion = all_of(profesion_col)) %>%
        count(Profesion) %>%
        arrange(desc(n)) %>%
        rename(`Número de Créditos` = n)
      
      #Tabla
      tab_profesion <- tabla %>%
        kable("html", escape = FALSE, booktabs = TRUE) %>%
        kable_styling(font_size = 11, full_width = FALSE) %>%
        row_spec(0, background = "#132b60", color = "#ffffff") %>%
        scroll_box(width = "100%", height = "400px")
      
      return(HTML(tab_profesion))
  }
  
  output$TablaEdades <- renderUI({
    req(datos())
    
    res04 <- datos() %>%
      mutate(
        FECHANACIMIENTO = as.Date(FECHANACIMIENTO),
        EDAD = floor(interval(FECHANACIMIENTO, Sys.Date()) / years(1)),
        GRUPO_EDAD = cut(
          EDAD,
          breaks = c(18, 24, 31, 39, 49, 59, 100),
          labels = c("18-24", "25-31", "32-39", "40-49", "50-59", "60-100"),
          right = FALSE
        ),
        RATIO_ENDEUDAMIENTO = MONTO_OTORGADO / TOTALINGRESOS
      ) %>%
      group_by(GRUPO_EDAD) %>%
      filter(
        TOTALINGRESOS >= quantile(TOTALINGRESOS, 0.02, na.rm = TRUE) &
          TOTALINGRESOS <= quantile(TOTALINGRESOS, 0.98, na.rm = TRUE)
      ) %>%
      summarise(
        INGRESO_PROMEDIO = mean(TOTALINGRESOS, na.rm = TRUE),
        MONTOCREDITO_PROMEDIO = mean(MONTO_OTORGADO, na.rm = TRUE),
        RATIO_ENDEUDAMIENTO_PROM = mean(RATIO_ENDEUDAMIENTO, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>%
      arrange(GRUPO_EDAD)
    
    tab04 <- res04 %>%
      kable(format = "html", digits = 2, align = "c") %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
      row_spec(0, background = "#132b60", color = "#ffffff")
    
    HTML(tab04)
  })
  
  #GRÁFICO DE PROFESIÓN
  output$GraficoProfesion <- renderPlot({
    req(datos())
      datos_df <- datos()
    
      profesion_col <- NULL
      for (col in names(datos_df)) {
        if (grepl("profesion", tolower(col))) {
          profesion_col <- col
          break
        }
      }
      
      if (is.null(profesion_col)) {
        stop("No se encontró columna de profesión")
      }
    
      datos_grafico <- datos_df %>%
        rename(Profesion = all_of(profesion_col)) %>%
        count(Profesion) %>%
        arrange(desc(n)) %>%
        head(10)  #Top 10 profesiones
      
      if (nrow(datos_grafico) == 0) {
        stop("No hay datos suficientes")
      }
      
      #Gráfico
      p <- ggplot(datos_grafico, aes(x = reorder(Profesion, n), y = n)) +
        geom_col(fill = "#132b60", alpha = 0.8) +
        labs(
          title = "Top 10 Profesiones con Más Créditos",
          x = "Profesión",
          y = "Número de Créditos"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        coord_flip()
      
      return(p)
      
      p_error <- ggplot(df_error, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5, color = "red") +
        theme_void() +
        labs(title = "No se pudo generar el gráfico")
      
      return(p_error)
  })
  output$GraficoEdades1 <- renderPlot({
    req(datos())
    
    res04 <- datos() %>%
      mutate(
        FECHANACIMIENTO = as.Date(FECHANACIMIENTO),
        EDAD = floor(interval(FECHANACIMIENTO, Sys.Date()) / years(1)),
        GRUPO_EDAD = cut(
          EDAD,
          breaks = c(18, 24, 31, 39, 49, 59, 100),
          labels = c("18-24", "25-31", "32-39", "40-49", "50-59", "60-100"),
          right = FALSE
        ),
        RATIO_ENDEUDAMIENTO = MONTO_OTORGADO / TOTALINGRESOS
      ) %>%
      group_by(GRUPO_EDAD) %>%
      filter(
        TOTALINGRESOS >= quantile(TOTALINGRESOS, 0.02, na.rm = TRUE) &
          TOTALINGRESOS <= quantile(TOTALINGRESOS, 0.98, na.rm = TRUE)
      ) %>%
      summarise(
        INGRESO_PROMEDIO = mean(TOTALINGRESOS, na.rm = TRUE),
        MONTOCREDITO_PROMEDIO = mean(MONTO_OTORGADO, na.rm = TRUE),
        RATIO_ENDEUDAMIENTO_PROM = mean(RATIO_ENDEUDAMIENTO, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(INGRESO_PROMEDIO, MONTOCREDITO_PROMEDIO),
                   names_to = "Variable", values_to = "Valor")
    
    ggplot(res04, aes(x = GRUPO_EDAD, y = Valor, fill = Variable)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#0072B2", "#E69F00")) +
      labs(title = "Ingreso vs Crédito Promedio por Grupo de Edad",
           x = "Grupo de Edad", y = "Promedio (USD)") +
      theme_minimal()
  })
  output$GraficoEdades3 <- renderPlot({
    req(datos())
    
    res04 <- datos() %>%
      mutate(
        FECHANACIMIENTO = as.Date(FECHANACIMIENTO),
        EDAD = floor(interval(FECHANACIMIENTO, Sys.Date()) / years(1)),
        GRUPO_EDAD = cut(
          EDAD,
          breaks = c(18, 24, 31, 39, 49, 59, 100),
          labels = c("18-24", "25-31", "32-39", "40-49", "50-59", "60-100"),
          right = FALSE
        )
      ) %>%
      group_by(GRUPO_EDAD) %>%
      summarise(N = n())
    
    ggplot(res04, aes(x = GRUPO_EDAD, y = N)) +
      geom_col(fill = "#0072B2") +
      geom_text(aes(label = N), vjust = -0.5) +
      labs(title = "Cantidad de Personas por Grupo de Edad",
           x = "Grupo de Edad", y = "Número de personas") +
      theme_minimal()
  })
  output$GraficoEdades4 <- renderPlot({
    req(datos())
    
    res04 <- datos() %>%
      mutate(
        FECHANACIMIENTO = as.Date(FECHANACIMIENTO),
        EDAD = floor(interval(FECHANACIMIENTO, Sys.Date()) / years(1)),
        GRUPO_EDAD = cut(
          EDAD,
          breaks = c(18, 24, 31, 39, 49, 59, 100),
          labels = c("18-24", "25-31", "32-39", "40-49", "50-59", "60-100"),
          right = FALSE
        ),
        RATIO_ENDEUDAMIENTO = MONTO_OTORGADO / TOTALINGRESOS
      ) %>%
      group_by(GRUPO_EDAD) %>%
      summarise(
        INGRESO_PROMEDIO = mean(TOTALINGRESOS, na.rm = TRUE),
        RATIO_ENDEUDAMIENTO_PROM = mean(RATIO_ENDEUDAMIENTO, na.rm = TRUE)
      )
    
    ggplot(res04, aes(x = GRUPO_EDAD)) +
      geom_col(aes(y = INGRESO_PROMEDIO), fill = "#0072B2", alpha = 0.6) +
      geom_line(aes(y = RATIO_ENDEUDAMIENTO_PROM * max(INGRESO_PROMEDIO),
                    group = 1), color = "#E69F00", size = 1.2) +
      geom_point(aes(y = RATIO_ENDEUDAMIENTO_PROM * max(INGRESO_PROMEDIO)),
                 color = "#E69F00", size = 3) +
      scale_y_continuous(
        name = "Ingreso Promedio (USD)",
        sec.axis = sec_axis(~./max(res04$INGRESO_PROMEDIO),
                            name = "Ratio de Endeudamiento")
      ) +
      labs(title = "Relación entre Ingreso y Ratio de Endeudamiento por Grupo de Edad",
           x = "Grupo de Edad") +
      theme_minimal()
  })
  
  archivo <- reactive({
    req(input$file)
    path <- input$file$datapath
    excel_sheets(path)
  })
  
  output$sheet_ui <- renderUI({
    req(archivo())
    selectInput("hoja", "Seleccionar hoja:", choices = archivo())
  })
  
  datos <- eventReactive(input$analizar, {
    req(input$file, input$hoja)
    read_excel(input$file$datapath, sheet = input$hoja)
  })
  
  
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste0("Informe_Estadistico_", Sys.Date(), ".html")  ## Cambio pdf a html
    },
    content = function(file) {
      temp_data <- tempfile(fileext = ".rds")
      saveRDS(datos(), temp_data)
      
      # Obtener las variables de entrada que definen los gráficos
      var_cual <- input$VarCual
      var_cuan1 <- input$VarCuan
      var_cuan2 <- input$VarCuan2
      color <- input$colx
      
      qmd_path <- normalizePath("reporte.qmd")
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Convertir QMD a RMD temporal
      file.copy(qmd_path, temp_rmd, , overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        params = list(data_path = temp_data,
                      var_cual_param = var_cual, # Variable cualitativa
                      var_cuan1_param = var_cuan1, # Variable cuantitativa 1
                      var_cuan2_param = var_cuan2, # Variable cuantitativa 2
                      color_param = color # Color
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  

}

