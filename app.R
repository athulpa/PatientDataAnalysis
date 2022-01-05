
# Link to hosted web app:
#   https://jkbcw3-athul-prakash.shinyapps.io/Project02/

# Libraries Used
library(ggplot2)
library(shiny)
library(shinydashboard)

## Import and set up the data
importData = function() {
  mydata = read.csv("data01.csv")
  cnames = c("age", "gendera", "BMI", "heart.rate", "Diastolic.blood.pressure", "diabetes", "Renal.failure", "Lymphocyte", "Neutrophils", "outcome")
  mydata=mydata[,cnames]
  colnames(mydata) = c("age", "gender", "BMI", "heart.rate", "BP", "diabetes", "renal.failure", "lymphocytes", "neutrophils", "outcome")
  # Remove patient with unknown outcome
  mydata = mydata[-1160,]
  
  ## Factor renaming
  mydata$gender[mydata$gender==1]="M"
  mydata$gender[mydata$gender==2]="F"
  mydata$gender = factor(mydata$gender)
  
  mydata$outcome[mydata$outcome==1]="YES"
  mydata$outcome[mydata$outcome==0]="NO"
  mydata$outcome = factor(mydata$outcome)
  
  mydata$diabetes[mydata$diabetes==1]="YES"
  mydata$diabetes[mydata$diabetes==0]="NO"
  mydata$diabetes = factor(mydata$diabetes)
  
  mydata$renal.failure[mydata$renal.failure==1]="YES"
  mydata$renal.failure[mydata$renal.failure==0]="NO"
  mydata$renal.failure = factor(mydata$renal.failure)
  mydata
}

mydata = importData()
head(mydata)

ui <- dashboardPage(
  dashboardHeader(title="Hospital Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Univariate Analysis", tabName = "singlevarmenu"),
      menuItem("Multivariate Analysis", tabName = "multivarmenu"),
      menuItem("[Diagnostics]", tabName = "diagmenu")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("singlevarmenu",
              tabsetPanel(
                tabPanel("Categorical Variables",
                         selectInput("catvar", "Categorical Variable", colnames(mydata)[c(2,6,7,10)]),
                         plotOutput("histplot"),
                         h4("Frequency Table"),
                         tableOutput("cattable")
                         ),
                tabPanel("Numeric Variables",
                         selectInput("numvar", "Numeric Variable", colnames(mydata)[c(1,3,4,5,8,9)]),
                         h4("Summary Statistics"),
                         tableOutput("summstat"),
                         h4("Plots"),
                         plotOutput("boxplot"),
                         plotOutput("qqplot")
                )
              )),
      tabItem("multivarmenu",
              tabsetPanel(
                tabPanel("Numeric-Numeric Analyis",
                         h3("Analyse 2 Numeric Variables against each other"),
                         selectInput("numvar1", "Variable 1", colnames(mydata)[c(1,3,4,5,8,9)]),
                         selectInput("numvar2", "Variable", colnames(mydata)[c(1,3,4,5,8,9)],selected="BMI"),
                         plotOutput("scatplot"),
                         actionButton("actSmooth", "Fit Regression Line")
                         ),
                tabPanel("Numeric-Categorical Analysis",
                         h3("Analyse a Numeric Variable against a Categorical Variable"),
                         selectInput("numvar3", "Numeric Variable", colnames(mydata)[c(1,3,4,5,8,9)]),
                         selectInput("catvar3", "Categorical Variable", colnames(mydata)[c(2,6,7,10)]),
                         plotOutput("fbplot")
                         ),
                tabPanel("Categorical-Categorical Analysis",
                         h3("Analyse 2 Categorical Variables against each other"),
                         selectInput("catvar1",  "Variable 1", colnames(mydata)[c(2,6,7,10)]),
                         selectInput("catvar2", "Variable 2", colnames(mydata)[c(2,6,7,10)], selected="diabetes"),
                         plotOutput("fhplot")
                )
              )),
      tabItem("diagmenu",
              tabsetPanel(
                tabPanel("Data Head",
                         tableOutput("tablehead")
                ),
                tabPanel("Data Select",
                         checkboxGroupInput("selvars", "Add/Remove Data Variables", colnames(mydata)),
                         textInput("selrows", "Select Rows (vector expression)", "1:15"),
                         tableOutput("tableselect")
                )
              ))
    )
  )
)

server <- function(input,output){
  
  rv = reactiveValues()
  
  output$histplot = renderPlot({
    var = input$catvar
    ggplot(data=mydata) + 
      layer(mapping=aes_string(x=var,fill=var), stat="count", geom="bar", position="identity") +
      ggtitle(paste0("Histogram of ", var))
  })
  
  output$cattable = renderTable({
    var = input$catvar
    t = table(mydata[,var])
  }, colnames=FALSE)
  
  output$summstat = renderTable({
    var = input$numvar
    s = summary(mydata[,var])
    df = data.frame(Min=s[[1]], Median=s[[3]], Mean=s[[4]], Max=s[[6]])
    df
  })
  
  output$boxplot = renderPlot({
    var = input$numvar
    ggplot(data=mydata, mapping=aes_string(x=var)) + geom_boxplot()
  })
  
  output$qqplot = renderPlot({
    var = input$numvar
    qqnorm(mydata[,var])
  })
  
  output$scatplot = renderPlot({
    var1 = input$numvar1
    var2 = input$numvar2
    g = ggplot(data=mydata, mapping=aes_string(x=var1,y=var2)) + geom_point(color="red") +
      ggtitle(paste0("Scatter-Plot of ", var1, " vs. ", var2))
    if(rv$line) {
      g = g + geom_smooth(method='lm')
    }
    g
  })
  
  observeEvent(input$numvar1, {
    rv$line = 0
  })
  
  
  observeEvent(input$numvar2, {
    rv$line = 0
  })
  
  observeEvent(input$actSmooth, {
    rv$line = 1
  })
  
  output$fbplot = renderPlot({
    varc = input$catvar3
    varn = input$numvar3
    ggplot(data=mydata) + geom_boxplot(mapping=aes_string(x=varc, y=varn, fill=varc)) +
      ggtitle(paste0("Box-Plot of ", varn, " Faceted by ", varc))
  })
  
  output$fhplot = renderPlot({
    var1 = input$catvar1
    var2 = input$catvar2
    ggplot(data=mydata) + geom_histogram(mapping=aes_string(x=var1, fill=var2), stat="count") +
      ggtitle(paste0("Histogram of ", var1, " stacked by ", var2))
  })
  
  output$tablehead = renderTable({
    mydata[1:10,]
  })
  
  output$tableselect = renderTable({
    vars = input$selvars
    dat = cbind(data.frame(S.no=1:nrow(mydata)), mydata[,vars])
    rows = eval(parse(text=input$selrows))
    dat[rows,]
  })
}


## main()
shinyApp(ui,server)
