is_package_installed <- function(package_name) {
  length(find.package(package_name, quiet = TRUE)) > 0
}
# Usage
if (!is_package_installed("shiny")) {
  install.packages("shiny")
}
library(shiny)


simulF<-function(fS, fV, fC, fMinCapconR,fMaxCapconR,fCCTrend, fMinEXP,fMaxEXP,fExpTrend, dType)
{
d<-10
##depreciation timeline
n<-100
##number of iterations
time<-c(1:n)
timescale <- matrix(nrow=6, ncol = n)
timex <- matrix(nrow=1, ncol = n)
timex<-c(sample(100, n, replace = TRUE))
row.names(timescale)<-c("S", "V", "C", "Investment", "Capitalist Consumption", "R")
timescale["S",1]<-fS
timescale["V",1]<-fV
timescale["C",1]<-fC
total<-sum(timescale["S",1], timescale["V",1], timescale["C",1] )

if ((fCCTrend=="opt1"))
{
  CCEnd<-min(fMinCapconR, fMaxCapconR)
}
else
{
  CCEnd<-max(fMinCapconR, fMaxCapconR)
}
capconratio<-matrix()
if (fMinCapconR==fMaxCapconR)
{
  capconratio[1:n]<-fMinCapconR
}
else
{
  capconratio<-c((sample((fMinCapconR*100):(fMaxCapconR*100), (n-20), replace = TRUE))/100, matrix(data = CCEnd, nrow = 20))
  capconratio<-sort(capconratio, decreasing = (fCCTrend=="opt1"))
}

timescale["Capitalist Consumption", 1]<-timescale["S",1]*capconratio[1]
timescale["Investment",1]<-timescale["S",1]-timescale["Capitalist Consumption", 1]
cstock<-matrix()
cstock[1:d]<-timescale["C",1]
##stock of capital
timescale["R", 1]<-timescale["S",1]/(timescale["V",1]+timescale["C",1])


if ((fExpTrend=="opt1"))
{
  expEnd<-min(fMinEXP, fMaxEXP)
}
else
{
  expEnd<-max(fMinEXP, fMaxEXP)
}
exp<-matrix()
if (fMinEXP==fMaxEXP)
{
  exp[1:n]<-fMinEXP
}
else
{
  exp<-c((sample((fMinEXP*100):(fMaxEXP*100), (n-20), replace = TRUE))/100, matrix(data = expEnd, nrow = 20))
  exp<-sort(exp, decreasing = (fExpTrend=="opt1"))
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
  titlePanel("Run Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("IntS", "Initial Surplus:", value = 30),
      numericInput("IntV", "Initial Variable Capital:", value = 65),
      numericInput("IntC", "Initial Constant Capital:", value = 5),
      numericInput("MinCapConR", "Minimum Rate of Capitalist Consumption (High=More Consumption):", value = .3, min = 0, max = 1, step = .05),
      numericInput("MaxCapConR", "Maximum Rate of Capitalist Consumption (High=More Consumption):", value = .3, min = 0, max = 1, step = .05),
      selectInput(
        inputId = "dropdown1",
        label = "Choose an option:",
        choices = list("Decreasing" = "opt1", "Increasing" = "opt2"),
        selected = "opt1"
      ),
      numericInput("MinExp", "Minimum Rate of Exploitation:", value = .3, min = 0, max = 1, step = .05),
      numericInput("MaxExp", "Maximum Rate of Exploitation:", value = .3, min = 0, max = 1, step = .05),
      selectInput(
        inputId = "dropdown2",
        label = "Choose an option:",
        choices = list("Decreasing" = "opt1", "Increasing" = "opt2"),
        selected = "opt1"
      ),
      selectInput(
        inputId = "dropdown",
        label = "Choose a Depreciation Type:",
        choices = list("Normal Depreciation" = "opt1", " Double Declining Balance Depreciation" = "opt2"),
        selected = "opt1"
      ),
      downloadButton("download_data", "Download Data"),
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

server <- function(input, output) {
  simulR<-reactive(
  simulF(input$IntS,input$IntV, input$IntC, input$MinCapConR, input$MaxCapConR,input$dropdown1, input$MinExp,input$MaxExp, input$dropdown2, input$dropdown)
  )
  # Download handler for the CSV file
  output$download_data <- downloadHandler(
    filename = function() {
      "simulation_data.csv"
    }
    ,
    content = function(file) {
      write.csv(simulR(), file)
    }
  )
  simulS<-reactive(
    simulR()["S",]
  )
  simulV<-reactive(
    simulR()["V",]
is_package_installed <- function(package_name) {
  length(find.package(package_name, quiet = TRUE)) > 0
}
# Usage
if (!is_package_installed("shiny")) {
  install.packages("shiny")
}
library(shiny)


simulF<-function(fS, fV, fC, fMinCapconR,fMaxCapconR,fCCTrend, fMinEXP,fMaxEXP,fExpTrend, dType)
{
d<-10
##depreciation timeline
n<-100
##number of iterations
time<-c(1:n)
timescale <- matrix(nrow=6, ncol = n)
timex <- matrix(nrow=1, ncol = n)
timex<-c(sample(100, n, replace = TRUE))
row.names(timescale)<-c("S", "V", "C", "Investment", "Capitalist Consumption", "R")
timescale["S",1]<-fS
timescale["V",1]<-fV
timescale["C",1]<-fC
total<-sum(timescale["S",1], timescale["V",1], timescale["C",1] )

if ((fCCTrend=="opt1"))
{
  CCEnd<-min(fMinCapconR, fMaxCapconR)
}
else
{
  CCEnd<-max(fMinCapconR, fMaxCapconR)
}
capconratio<-matrix()
if (fMinCapconR==fMaxCapconR)
{
  capconratio[1:n]<-fMinCapconR
}
else
{
  capconratio<-c((sample((fMinCapconR*100):(fMaxCapconR*100), (n-20), replace = TRUE))/100, matrix(data = CCEnd, nrow = 20))
  capconratio<-sort(capconratio, decreasing = (fCCTrend=="opt1"))
}


timescale["Capitalist Consumption", 1]<-timescale["S",1]*capconratio[1]
timescale["Investment",1]<-timescale["S",1]-timescale["Capitalist Consumption", 1]
cstock<-matrix()
cstock[1:d]<-timescale["C",1]
##stock of capital
timescale["R", 1]<-timescale["S",1]/(timescale["V",1]+timescale["C",1])


if ((fExpTrend=="opt1"))
{
  expEnd<-min(fMinEXP, fMaxEXP)
}
else
{
  expEnd<-max(fMinEXP, fMaxEXP)
}
exp<-matrix()
if (fMinEXP==fMaxEXP)
{
  exp[1:n]<-fMinEXP
}
else
{
  exp<-c((sample((fMinEXP*100):(fMaxEXP*100), (n-20), replace = TRUE))/100, matrix(data = expEnd, nrow = 20))
  exp<-sort(exp, decreasing = (fExpTrend=="opt1"))
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
      HTML("<br></br><b><h4>Set Rates of Capitalist Consumption and Exploitation. Must be between 0 and 1.</h4></b>"),
      numericInput("MinCapConR", "Minimum Rate of Capitalist Consumption (High=More Consumption):", value = .3, min = 0, max = 1, step = .05),
      numericInput("MaxCapConR", "Maximum Rate of Capitalist Consumption (High=More Consumption):", value = .3, min = 0, max = 1, step = .05),
      selectInput(
        inputId = "dropdown1",
        label = "Choose whether the rate of capitalist consumption will decrease/increase:",
        choices = list("Decreasing" = "opt1", "Increasing" = "opt2"),
        selected = "opt1"
      ),
      numericInput("MinExp", "Minimum Rate of Exploitation:", value = .3, min = 0, max = 1, step = .05),
      numericInput("MaxExp", "Maximum Rate of Exploitation:", value = .3, min = 0, max = 1, step = .05),
      selectInput(
        inputId = "dropdown2",
        label = "Choose whether the rate of exploitation will decrease/increase:",
        choices = list("Decreasing" = "opt1", "Increasing" = "opt2"),
        selected = "opt1"
      ),
      selectInput(
        inputId = "dropdown",
        label = "Choose a Depreciation Type:",
        choices = list("Normal Depreciation" = "opt1", " Double Declining Balance Depreciation" = "opt2"),
        selected = "opt1"
      ),
      downloadButton("download_data", "Download Data"),
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
    if (input$MinCapConR < 0) {
      updateNumericInput(session, "MinCapConR", value = 0)
    } else if (input$MinCapConR > 1) {
      updateNumericInput(session, "MinCapConR", value = 1)
    }
    if (input$MaxCapConR < 0) {
      updateNumericInput(session= getDefaultReactiveDomain(), "MaxCapConR", value = 0)
    } else if (input$MaxCapConR > 1) {
      updateNumericInput(session, "MaxCapConR", value = 1)
    }
    if (input$MinExp < 0) {
      updateNumericInput(session, "MinExp", value = 0)
    } else if (input$MinExp > 1) {
      updateNumericInput(session, "MinExp", value = 1)
    }
    if (input$MaxExp < 0) {
      updateNumericInput(session, "MaxExp", value = 0)
    } else if (input$MaxExp > 1) {
      updateNumericInput(session, "MaxExp", value = 1)
    }
  })
  
  simulR<-reactive({
    
  simulF(input$IntS,input$IntV, input$IntC, input$MinCapConR, input$MaxCapConR,input$dropdown1, input$MinExp,input$MaxExp, input$dropdown2, input$dropdown)
  })
  # Download handler for the CSV file
  output$download_data <- downloadHandler(
    filename = function() {
      "simulation_data.csv"
    }
    ,
    content = function(file) {
      write.csv(simulR(), file)
    }
  )
  simulS<-reactive(
    simulR()["S",]
  )
  simulV<-reactive(
    simulR()["V",]
  )
  simulC<-reactive(
    simulR()["C",]
  )
  simulRoP<-reactive(
    simulR()["R",]
  )
  simulExp<-reactive(
    simulR()[7,]
  )
  simulInv<-reactive(
    simulR()["Investment",]
  )
  simulCC<-reactive(
    simulR()["Capitalist Consumption",]
  )
  simulCCR<-reactive(
    simulR()[8,]
  )
  simulIR<-reactive(
    simulC()/simulInv()
  )
  output$result1 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MaxCapConR!="", input$MinExp!="",input$MaxExp!="", cancelOutput = TRUE)
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulS(), type="l", xlab = "time",ylim = c(0,100),ylab = "", main = "Surplus Breakdown")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    lines(simulInv(), col = "red")
    lines(simulCC(), col = "blue")
    legend("topright", legend = c("Surplus", "Gross Investment", "Capitalist Consumption"), col = c("black", "red", "blue"), lty = 1, cex = 0.8)
    })
  output$result2 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MaxCapConR!="", input$MinExp!="",input$MaxExp!="", cancelOutput = TRUE)
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulV(), type="l", xlab = "time",ylim = c(0,100), ylab = "", main = "Variable Capital and Capitalist Consumption")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    lines(simulCC(), col = "blue")
    legend("topright", legend = c("Variable Capital", "Capitalist Consumption"), col = c("black", "blue"), lty = 1, cex = 0.8)
    })
  output$result3 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MaxCapConR!="", input$MinExp!="",input$MaxExp!="", cancelOutput = TRUE)
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulC(), type="l", xlab = "time",ylim = c(0,100), ylab = "", main = "Constant Capital and Investment")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    lines(simulInv(), col = "red")
    legend("topright", legend = c("Constant Capital/Depreciation", "Gross Investment"), col = c("black", "red"), lty = 1, cex = 0.8)
    })
  output$result4 <- renderPlot({
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MaxCapConR!="", input$MinExp!="",input$MaxExp!="", cancelOutput = TRUE)
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    plot(simulRoP(), type="l", xlab = "time", ylim = c(0,max(c(simulRoP(),1) )), ylab = "", main = "Rates of Profit and Exploitation")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)                           
    lines(simulExp(), col = "red")
                               legend("topright", legend = c("Rate of Profit", "Rate of Exploitation"), col = c("black", "red"), lty = 1, cex = 0.8)
                               })
  output$result5 <- renderPlot({
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MaxCapConR!="", input$MinExp!="",input$MaxExp!="", cancelOutput = TRUE)
    plot(simulCCR(), type="l", xlab = "time", ylab = "",ylim = c(0,1), main = "Rate of Capitalist Consumption")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    })
  output$result6 <- renderPlot({
    par(font.main = 2, font.lab = 2, font.axis = 2, cex = 1.2)
    req(input$IntS!="",input$IntV!="", input$IntC!="", input$MinCapConR!="", input$MaxCapConR!="", input$MinExp!="",input$MaxExp!="", cancelOutput = TRUE)
    plot(simulIR(), type="l", xlab = "time", ylab = "", main = "Ratio of Gross Investment to Constant Capital")
    grid(nx= NA , ny=NULL, col = "gray", lty = "dotted", equilogs = FALSE)
    })

}
shinyApp(ui = ui, server = server)
