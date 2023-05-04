is_package_installed <- function(package_name) {
  length(find.package(package_name, quiet = TRUE)) > 0
}
# Usage
if (!is_package_installed("shiny")) {
  install.packages("shiny")
}
library(shiny)


simulF<-function(fS, fV, fC, fMinCapconR, fMinEXP, dType)
{
  d<-10
  ##depreciation timeline
  n<-1000
  ##number of iterations
  time<-c(1:n)
  timescale <- matrix(nrow=6, ncol = n)

  row.names(timescale)<-c("S", "V", "C", "Investment", "Capitalist Consumption", "R")
  timescale["S",1]<-fS
  timescale["V",1]<-fV
  timescale["C",1]<-fC
  total<-sum(timescale["S",1], timescale["V",1], timescale["C",1] )
  
    prob1<-c(1:((fMinCapconR*100)+100), 1:(201-((fMinCapconR*100)+100)) )
    prob1<-prob1[1:200]
    prob1<-prob1/sum(prob1)
    capconratio1<-((sample(1:200, n, replace = TRUE, prob = prob1 ))-100)/100
    tick<-2
    capconratio[1:n]<-.5
    while (tick<= n)
    {
      capconratio[tick]<-capconratio[tick-1]*(1+capconratio1[tick])
      if (capconratio[tick]>1)
      {
        capconratio[tick]<-1
      }
      if (capconratio[tick]<.01)
      {
        capconratio[tick]<-.01
      }
      tick<-tick+1
    }

  
  
  
  timescale["Capitalist Consumption", 1]<-timescale["S",1]*capconratio[1]
  timescale["Investment",1]<-timescale["S",1]-timescale["Capitalist Consumption", 1]
  cstock<-matrix()
  cstock[1:d]<-timescale["C",1]
  ##stock of capital
  timescale["R", 1]<-timescale["S",1]/(timescale["V",1]+timescale["C",1])
  

  prob2<-c(1:((fMinEXP*100)+100), 1:(201-((fMinEXP*100)+100)) )
  prob2<-prob2[1:200]
  prob2<-prob2/sum(prob2)
  exp1<-(sample(1:200, n, replace = TRUE, prob = prob1 )-100)/100
  tick<-2
  exp[1:n]<-.5
  while (tick<= n)
  {
    exp[tick]<-exp[tick-1]*(1+exp1[tick])
    if (exp[tick]>1)
    {
      exp[tick]<-1
    }
    if (exp[tick]<.01)
    {
      exp[tick]<-.01
    }
    tick<-tick+1
  }
  t<-2
  while (t<=n)
  {
    
    timescale["C", t]<-round(cstock[1], digits =3);
    timescale["S", t]<-round((total-timescale["C", (t)])*((exp[t])), digits = 3);
    timescale["V", t]<-round((total-timescale["C", (t)])*(1-(exp[t])), digits = 3);
    timescale["Capitalist Consumption", t]<-round((timescale["S",t]+timescale["C", t])*capconratio[t], digits =3);
    timescale["Investment",t]<-round(timescale["S",t]+timescale["C", t]-timescale["Capitalist Consumption", t], digits = 3);
    timescale["R", t]<- round(timescale["S",t]/(timescale["V",t]+timescale["C",t]), digits = 3);
    inv1<-timescale["Investment",t];
    cstock<-c(cstock[2:d],0 )
    if(dType=="opt2")
    {
      dp1<-1
      dtemp<-0
      for (dep in cstock)
      {
        if (dp1<d)
        {
          cstock[dp1]<-dep+round(((2*(timescale["Investment",t]-dtemp))/d), digits = 3);
          dtemp<-dtemp+round(((2*(timescale["Investment",t]-dtemp))/d), digits = 3);
          dp1<-dp1+1;
        }
        else
        {
          cstock[dp1]<-dep+round((timescale["Investment",t]-dtemp), digits = 3)
        }
      }
    }
    else
    {
      cstock<-round(cstock+(timescale["Investment",t]/d), digits = 3);
    }
    
    t<-t+1
  }
  timescale1<-rbind(timescale, exp, capconratio)
  timescale1
}



# Define UI
ui <- fluidPage(
  titlePanel("Rate of Profit Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      HTML("<br></br><b><h4>Set starting values.</h4></b>"),
      numericInput("IntS", "Initial Surplus:", value = 30),
      numericInput("IntV", "Initial Variable Capital:", value = 65),
      numericInput("IntC", "Initial Constant Capital:", value = 5),
      HTML("<br></br><b><h4>Set Bias of Capitalist Consumption and Exploitation. Must be between -1 and 1.</h4></b>"),
      numericInput("MinCapConR", "Bias of Change in Capitalist Consumption (High=More Consumption):", value = .3, min = -1, max = 1, step = .05),
      numericInput("MinExp", "Bias of Change in Exploitation:", value = .3, min = -1, max = 1, step = .05),
      selectInput(
        inputId = "dropdown",
        label = "Choose a Depreciation Type:",
        choices = list("Normal Depreciation" = "opt1", " Double Declining Balance Depreciation" = "opt2"),
        selected = "opt1"
      ),
      downloadButton("download_data", "Download Data"),
      actionButton("submit_btn", "Submit"),
    ),
    
    mainPanel(
      plotOutput("result6"),
      plotOutput("result4"),
      plotOutput("result1"),
      plotOutput("result2"),
      plotOutput("result3"),
      plotOutput("result5"),
    )
  )
)

server <- function(input, output, session) {
  # Enforce the minimum and maximum values for each numeric input
  observe({
    if (input$MinCapConR < -1) {
      updateNumericInput(session, "MinCapConR", value = -1)
    } else if (input$MinCapConR > 1) {
      updateNumericInput(session, "MinCapConR", value = 1)
    }

    if (input$MinExp < -1) {
      updateNumericInput(session, "MinExp", value = -1)
    } else if (input$MinExp > 1) {
      updateNumericInput(session, "MinExp", value = 1)
    }

  })

  observeEvent(input$submit_btn, {
    simulR<-(
    simulF(input$IntS,input$IntV, input$IntC, input$MinCapConR, input$MinExp, input$dropdown)
  )
      # Download handler for the CSV file
  output$download_data <- downloadHandler(
    filename = function() {
      "simulation_data.csv"
    }
    ,
    content = function(file) {
      write.csv(simulR, file)
    }
  )
 
  simulS<-(
    simulR["S",]
  )
  simulV<-(
    simulR["V",]
  )
  simulC<-(
    simulR["C",]
  )
  simulRoP<-(
    simulR["R",]
  )
  simulExp<-(
    simulR[7,]
  )
  simulInv<-(
    simulR["Investment",]
  )
  simulCC<-(
    simulR["Capitalist Consumption",]
  )
  simulCCR<-(
    simulR[8,]
  )
  simulIR<-(
    simulC/simulInv
  )
  output$result1 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MinExp!="", cancelOutput = TRUE)
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulS, type="l", xlab = "time",ylim = c(0,100),ylab = "", main = "Surplus Breakdown")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    lines(simulInv, col = "red")
    lines(simulCC, col = "blue")
    legend("topright", legend = c("Surplus", "Gross Investment", "Capitalist Consumption"), col = c("black", "red", "blue"), lty = 1, cex = 0.8)
  })
  output$result2 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MinExp!="", cancelOutput = TRUE)    
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulV, type="l", xlab = "time",ylim = c(0,100), ylab = "", main = "Variable Capital and Capitalist Consumption")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    lines(simulCC, col = "blue")
    legend("topright", legend = c("Variable Capital", "Capitalist Consumption"), col = c("black", "blue"), lty = 1, cex = 0.8)
  })
  output$result3 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MinExp!="", cancelOutput = TRUE)    
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulC, type="l", xlab = "time",ylim = c(0,100), ylab = "", main = "Constant Capital and Investment")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    lines(simulInv, col = "red")
    legend("topright", legend = c("Constant Capital/Depreciation", "Gross Investment"), col = c("black", "red"), lty = 1, cex = 0.8)
  })
  output$result4 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MinExp!="", cancelOutput = TRUE)    
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulRoP, type="l", xlab = "time", ylim = c(0,max(c(simulRoP,1) )), ylab = "", main = "Rates of Profit and Exploitation")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)                           
    lines(simulExp, col = "red")
    legend("topright", legend = c("Rate of Profit", "Rate of Exploitation"), col = c("black", "red"), lty = 1, cex = 0.8)
  })
  output$result5 <- renderPlot({
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MinExp!="", cancelOutput = TRUE)    
    plot(simulCCR, type="l", xlab = "time", ylab = "",ylim = c(0,1), main = "Rate of Capitalist Consumption")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
  })
  output$result6 <- renderPlot({
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MinExp!="", cancelOutput = TRUE)    
    plot(simulIR, type="l", xlab = "time", ylab = "", main = "Ratio of Constant Capital to Gross Investment")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
  })
})
}
shinyApp(ui = ui, server = server)
