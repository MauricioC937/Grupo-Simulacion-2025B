
library(shiny)

# Método multiplicativo
mm <- function(x0,a,m){
  return( (a*x0) %% m)
}
mm(x0=79,a=153,m=97)

mm_sim <- function(x0,a,m,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(k in 1:nsim){
    vec[k+1] <- mm(vec[k],a,m)
  }
  vec <- (vec[-1])/m # retirar la observación x0
  return(vec)
}

### Método mixto
mi <- function(x0,a,m,c){
  return( (a*x0+c) %% m)
}


mi_sim <- function(x0,a,m,c,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(k in 1:nsim){
    vec[k+1] <- mi(vec[k],a,m,c)
  }
  vec <- (vec[-1])/m # retirar la observación x0
  return(vec)
}
#Cuadrados medios
mc <- function(x0,k){
  return(floor((x0^2-floor((x0^2)/(10^(2*k-k/2)))*10^(2*k-k/2))/(10^(k/2))))
}
mc(x0=89,k=2)
mc_sim <- function(x0,k,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(j in 1:nsim){
    vec[j+1] <- mc(vec[j],k)
  }
  vec <- vec[-1]/(10^k) # retiramos x0
  return(vec)
}
### Método Lehmer
ml <- function(x0,n,c){
  return((x0*c-floor(x0*c/10^n)*10^n)-floor(x0*c/10^n))
}

ml(x0=4122,n=4,c=76)
ml_sim <- function(x0,n,c,nsim){
  vec <- numeric(nsim+1)
  vec[1] <- x0
  for(j in 1:nsim){
    vec[j+1] <- ml(vec[j],n,c)
  }
  vec <- vec[-1]/(10^n) # retiramos x0
  return(vec)
}

# Define server logic required to draw a histogram
function(input, output, session) {
  
  fx <- reactive({
    expresion <- input$func
    #Construye una función a partir del texto
    function(x){
      eval(parse(text=expresion))
    }
  })
  
  aleatorios <- eventReactive(input$simular,{
    if(input$metodo=='Congruencial multiplicativo'){
      res <- mm_sim(x0=input$x0,a=input$a,m=input$m,nsim=input$num)
    }else if(input$metodo=='Congruencial mixto'){
      res <- mi_sim(x0=input$x0,a=input$a,m=input$m,c=input$c,nsim=input$num)
    }else if(input$metodo=='Cuadrados Medios'){
      res <- mc_sim(x0=input$x0,k=nchar(input$x0),nsim=input$num)
    }else{
      res <- ml_sim(x0=input$x0,n=4,c=2,nsim=input$num)
    }
    res
      })

    
    output$code <- renderPrint({
      aleatorios()
      })
    output$grafico <- renderPlot({
      hist(aleatorios(),breaks=10,col='#B22747',main='Números aleatorios')
    })
    output$resumen <- renderPrint({
      summary(aleatorios())
    })
    output$graficofun <- renderPlot({
      xvals <- seq(input$linf,input$lsup,length.out=100)
      yvals <- fx()(xvals)
      
      plot(xvals,yvals,type='l',col='red',main='Función ingresada por el usuario')
    })
    output$integral <- renderPrint({
      hy <- (input$lsup -input$linf)*fx()(input$linf+(input$lsup -input$linf)*aleatorios)
      mean(hy)
    })

}
