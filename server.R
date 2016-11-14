library(shiny)
library(ggplot2)
require(RColorBrewer)

source("Simul_fun.r")
#myCols <- brewer.pal(6,"Set1")
#display.brewer.all(n=NULL, type="div", select=NULL, exact.n=TRUE,colorblindFriendly=T)


# Define server logic 
#
shinyServer(function(input, output) {
  
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  randomVals <- eventReactive(input$go, {
    TRUE
  })
  
  mdl <- reactive({
#    if(randomVals()){
      
    # Build a list with parameters
    pars <-c(r=input$r,s=input$s,a=input$a,omega=2*pi/input$T)
    xstart <-  c(time=0,N=input$N0)
    times <- seq(0,input$tt,by=1)
    set.seed(input$rseed)    
    nsims<-input$nsims
    
    # Run simulations of the stochastic model
    #
    simdat <- vector(mode='list',length=nsims)
    for (k in 1:nsims) {
      simdat[[k]] <- STO_logistic_ef(xstart,pars,times)
    }

    # logistica determinista
    #
    require(deSolve)
    yini  <- c(N = input$N0)
    times <- seq(0,input$tt,by=1)
    out   <- ode(yini, times, logistic_ef_det, pars)
  
    list(simdat=simdat,out=out)
#    }
  })
  
  output$modelPlot <- renderPlot({
    
#  if(randomVals()){
      # Build a list with paramete  if(randomVals()){

      pars <-c(r=input$r,s=input$s)
      nsims<-input$nsims
      

      simdat <- mdl()[["simdat"]]      

      myCols <- brewer.pal(nsims,"Paired")
      
      # Range for plots
      #
      trange <- range(0:input$tt)
      nrange <- range(sapply(simdat,function(x)range(x$N)))
      

      # logistica determinista
      #
      out <- mdl()[["out"]]       
      if(max(out[,2]) > max(nrange))
        nrange <- range(out[,2])

      # Plot
      #
      title <- paste0("r=",pars[1]," K= ",round(pars[1]/pars[2],2))
      plot(trange,nrange,type='n',xlab='time',ylab="Pop",bty='l',main=title)
      for (k in 1:nsims)
        lines(N~time,data=simdat[[k]],col=myCols[k],type='l',pch=16)
        
      lines(N~time,data=out,type='l',lwd=.5, col="black", lty="dashed")
        
#    }

  })
  
  output$K <- 
    renderText(paste('Carrying capacity (K): ', round(input$r/input$s,2)))
  
  output$omega <- 
    renderText(paste('Angular Frequency : ', round(2*pi/input$T,2)))
  
  output$datatable <- renderTable({
#    if(randomVals()){
      
      simdat <- mdl()[["simdat"]]      
      
      df <- do.call("cbind", simdat)
#    }
  })
  
})

