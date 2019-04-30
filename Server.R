#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinydashboard)
library(micromapST)
library(glmnet)
library(randomForest)
library(DAAG)
library(splines)
library(ggplot2)
library(tidyverse)

setwd('D:\\bin\\STAT-515\\Final_Project\\Datasets')
indiaData<-read.csv('cancer-2.csv')
colnames(indiaData)

namesList=list(
  States = "Name of the state",
  Pop.2013.2014. = "Population of indian states",
  Rural.2013.2014. = "Rural population of Indian states",
  Urban.2013.2014. = "Urban population of Indian states",
  GDP.2013.14. = "GDP of Indian states",
  PovertyPercentage.2011.12. = "Percentage of population below",
  Averege.Population.Served.by.Govt..Doctors = "Average population served",
  DeathsByTb.2012 = "Death by tuberculosis",
  DeathsByTb.2013 = "Death by tuberculosis",
  DeathsByTb.2014 = "Death by tuberculosis",
  Institutional.Births = "Childern born in Institutions",
  X ="X",
  IMRper1000Births.2016 ="Infant Mortality Rate",
  IMRper1000Births.2015 ="Infant Mortality rate",
  IMRper1000.2014 = "Infant Mortality Rate",
  IMRper1000Births.2013 ="Infant Mortality Rate",
  IMRper1000Birth.2012 ="Infant Mortality Rate",
  IMRper1000Births.2011 ="Infant Mortality Rate",
  IMRper1000Births.2010 ="Infant Mortality Rate",
  IMRper1000Births.2009 ="Infant Mortality Rate",
  IMRper1000Births.2008 ="Infant Mortality Rate",
  IMRper1000Births.2007 ="Infant Mortality Rate",
  IMRper1000Births.2006="Infant Mortality Rate",
  IMRper1000Births.2005="Infant Mortality Rate",                   
  IMRper1000Births.2004="Infant Mortality Rate",                    
  IMRper1000Births.2003="Infant Mortality Rate",                    
  IMRper1000Births.2002="Infant Mortality Rate",                     
  IMRper1000Births.2001="Infant Mortality Rate",                     
  IMRper1000Births.2000="Infant Mortality Rate",                      
  State.Size.Classification="Size or Classification",                
  Suicidal.Deaths="Number of Suicidal Deaths",                           
  TotalBirthRate2014="Total Birth Rate",                         
  RuralBirthRate2014="Rural Birth Rate",                        
  UrbanBirthRate2014="Urban Birth Rate", 
  TotalBirthRate2015="Total Birth Rate",                         
  RuralBirthRate2015="Rural Birth Rate",                        
  UrbanBirthRate2015="Urban Birth Rate",
  TotalBirthRate2016="Total Birth Rate",                         
  RuralBirthRate2016="Rural Birth Rate",                        
  UrbanBirthRate2016="Urban Birth Rate",
  Region="Region that the state ",                                    
  GER.HigherEdMale="Gross Enrollment Ratio Males",                           
  GER.HigherEdFemale="Gross Enrollment Ratio Females",                        
  GER.HigherEdTotal="Total Gross Enrollment Ratio",
  crimeRate="Crime Rate per",
  Literacy.2011="Literacy Rate in",
  Higher_Secondary_Total="Percentage of people who have",
  PerCapita="Per capita income")

namesList1=list(
  States = "name of the state",
  Pop.2013.2014. = "(2013 - 2014)(Count)",
  Rural.2013.2014. = "(2013-2014)(%)",
  Urban.2013.2014. = "(2013 - 2014)(%)",
  GDP.2013.14. = "(2013-2014)(Rupees-Rs)",
  PovertyPercentage.2011.12. = "below poverty line(2011 and 2012)(%)",
  Averege.Population.Served.by.Govt..Doctors = "by government doctors",
  DeathsByTb.2012 = "2012 (count)",
  DeathsByTb.2013 = "2013 (count)",
  DeathsByTb.2014 = "2014 (count)",
  Institutional.Births = "(%)-2016",
  X ="X",
  IMRper1000Births.2016 ="per 1000 births-2016",
  IMRper1000Births.2015 ="per 1000 births-2015",
  IMRper1000.2014 = "per 1000 births-2014",
  IMRper1000Births.2013 ="per 1000 births-2013",
  IMRper1000Birth.2012 ="per 1000 births-2012",
  IMRper1000Births.2011 ="per 1000 births-2011",
  IMRper1000Births.2010 ="per 1000 births-2010",
  IMRper1000Births.2009 ="per 1000 births-2009",
  IMRper1000Births.2008 ="per 1000 births-2008",
  IMRper1000Births.2007 ="per 1000 births-2007",
  IMRper1000Births.2006="per 1000 Births-2006",
  IMRper1000Births.2005="per 1000 Births-2005",                   
  IMRper1000Births.2004="per 1000 Births-2004",                    
  IMRper1000Births.2003="per 1000 Births-2003",                    
  IMRper1000Births.2002="per 1000 Births-2002",                     
  IMRper1000Births.2001="per 1000 Births-2001",                     
  IMRper1000Births.2000="per 1000 Births-2000",                      
  State.Size.Classification="Size or Classification of the State",                
  State.UT="State or Union Territory",                                   
  Suicidal.Deaths="Number of Suicidal Deaths",                           
  TotalBirthRate2014="-2014",                         
  RuralBirthRate2014="-2014",                        
  UrbanBirthRate2014="-2014", 
  TotalBirthRate2015="-2015",                         
  RuralBirthRate2015="-2015",                        
  UrbanBirthRate2015="-2015",
  TotalBirthRate2016="-2016",                         
  RuralBirthRate2016="-2016",                        
  UrbanBirthRate2016="-2016",
  Region="Region in the State",                                    
  GER.HigherEdMale="in Higher Education(%)",                           
  GER.HigherEdFemale="in Higher Education(%)",                        
  GER.HigherEdTotal="in Higher Education(%)",
  crimeRate="100,000 people",
  Literacy.2011="2011(%)",
  Higher_Secondary_Total="passed high school(%)",
  PerCapita="Rupees")






shinyServer(function(input, output,session) {
  hideTab(inputId = "tabs", target = "Reference")

  output$distPlot <- renderPlotly({
    x <- list(
      title = input$X
      
    )
    y <- list(
      title = input$Y
      
    )

    plot_ly(data = indiaData, x = ~get(input$X), y =~get(input$Y) ,
            color = ~get(input$Classification), colors = c('red','#FF8C00','#7CFC00','#FF69B4','#87421F','#0000FF'),text=indiaData$States,
            marker = list(size = 10,
                          line = list(color = 'rgba(152, 0, 0, .8)',
                                      width = 2)),type="scatter"
                                        )%>%
      layout(xaxis = x, yaxis = y)
            
            
            


  })


  observeEvent(input$go, {
    #showTab(inputId = "tabs", target = "Reference")
    col3Input<-input$col3
    col4Input<-input$col4
    lab13=get(col3Input, namesList)
    lab14=get(col4Input, namesList)
    lab23=get(col3Input, namesList1)
    lab24=get(col4Input, namesList1)
    print(lab13)
    panelDesc <- data.frame(
      type=c(input$mapTypes,'id',input$plotTypeCol3,input$plotTypeCol4),
      lab1=c('','',lab13,lab14),
      lab2=c('' ,'',lab23,lab24),
      col1 = c(NA,NA,input$col3,input$col4)
    )

    fName = "D:\\bin\\STAT-515\\Final_Project\\Shiny\\www\\Indian_Micromap37.pdf"
    pdf(file=fName,width=7.5,height=10)
    micromapST(indiaData, panelDesc,
               rowNamesCol=1,
               rowNames='full',
               bordGrp="IndStatesNBG",bordDir="D:\\bin\\STAT-515\\Final_Project",
               sortVar=input$sorVar,ascend=FALSE,
               title=c("Analysis of Economy and",
                       " Health Indicators of Indian States"),
               ignoreNoMatches=TRUE)
    dev.off()
    updateTabItems(session, inputId="tabs", selected = "Reference")
    showTab(inputId = "tabs", target = "Reference")

    output$fetchPDF <- renderUI({

      tabsetPanel(
        # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
        tabPanel("Reference",
                 tags$iframe(style="height:850px; width:100%; scrolling=yes",
                             src="Indian_Micromap37.pdf")),id="tabs"

      )
  })



  }
  )

  observeEvent(input$interactionBinary,{
    if(input$interactionBinary=='Yes')
    {
      print("Inside Render UI")
      output$fetchInteractOptions<-renderUI({

      selectInput("interactOptions","Interaction Terms",
                                      colnames(indiaData),
                                              selected = "States",multiple = TRUE)})
    }
    else
    {
      output$fetchInteractOptions<-NULL
    }
    }
  )
  
  observeEvent(input$polyBinary,{
    if(input$polyBinary=='Yes')
    {
      print("Inside Render UI")
      output$fetchPolyOptions<-renderUI({
        
        selectInput("polyOptions","Polynomial Terms",
                    colnames(indiaData),
                    selected = "States",multiple = TRUE)})
    }
    else
    {
      output$fetchInteractOptions<-NULL
    }
  }
  )
  observeEvent(input$splineBinary,{
    if(input$splineBinary=='Yes')
    {
      print("Inside Render UI")
      output$fetchSplineOptions<-renderUI({
        
        selectInput("splineOptions","Spline Terms",
                    colnames(indiaData),
                    selected = "States",multiple = FALSE)
    })
      
      output$fetchSplineOptions1<-renderUI({
        
        selectInput("splineOptions1","Number",
                    c(1:10),
                    selected =1,multiple = FALSE)
      })
    }
    else
    {
      output$fetchInteractOptions<-NULL
    }
  }
  )
  runRegression <- reactive({
    if(input$interactionBinary=='Yes' && input$polyBinary=='Yes' && input$splineBinary=='No')
    {
      print(paste("+poly(", input$polyOptions,",2)"))
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+", input$interactOptions, collapse = "*"),paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
    }
    else if(input$interactionBinary=='Yes' && input$polyBinary=='No'&& input$splineBinary=='No'){
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+", input$interactOptions, collapse = "*"))),data=indiaData)
    }
    else if(input$interactionBinary=='No' && input$polyBinary=='Yes'&& input$splineBinary=='No'){
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
    }
    else if(input$interactionBinary=='No' && input$polyBinary=='No'&& input$splineBinary=='Yes'){
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"))),data=indiaData)
    }
    else if(input$interactionBinary=='No' && input$polyBinary=='Yes'&& input$splineBinary=='Yes'){
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"), paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
    }
    else if(input$interactionBinary=='Yes' && input$polyBinary=='Yes'&& input$splineBinary=='Yes'){
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"), paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
    }
    else
    {
      lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"))),data=indiaData)
    }
  })
  
  runRegressionCv <- reactive({
    if(input$interactionBinary=='Yes' && input$polyBinary=='Yes' && input$splineBinary=='No')
    {
      #print(paste("+poly(", input$polyOptions,",2)"))
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+", input$interactOptions, collapse = "*"),paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
      cv.lm(data = indiaData, form.lm = formula(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+", input$interactOptions, collapse = "*"),paste("+poly(", input$polyOptions,",2)")))), m=5, dots = 
              FALSE, seed=29, plotit=FALSE, printit=TRUE)
    }
    else if(input$interactionBinary=='Yes' && input$polyBinary=='No'&& input$splineBinary=='No'){
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+", input$interactOptions, collapse = "*"))),data=indiaData)
      cv.lm(data = indiaData, form.lm = formula(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+", input$interactOptions, collapse = "*")))), m=5, dots = 
              FALSE, seed=29, plotit=FALSE, printit=FALSE)
    }
    else if(input$interactionBinary=='No' && input$polyBinary=='Yes'&& input$splineBinary=='No'){
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
      cv.lm(df = indiaData, form.lm = formula(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+poly(", input$polyOptions,",2)")))), m=5, dots = 
              FALSE, seed=29, plotit=FALSE, printit=TRUE)
    }
    else if(input$interactionBinary=='No' && input$polyBinary=='No'&& input$splineBinary=='Yes'){
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"))),data=indiaData)
      cv.lm(data = indiaData, form.lm = formula(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")")))), m=5, dots = 
              FALSE, seed=29, plotit=FALSE, printit=TRUE)
    }
    else if(input$interactionBinary=='No' && input$polyBinary=='Yes'&& input$splineBinary=='Yes'){
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"), paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
      cv.lm(data = indiaData, form.lm = formula(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"), paste("+poly(", input$polyOptions,",2)")))), m=5, dots = 
              FALSE, seed=29, plotit=FALSE, printit=TRUE)
    }
    else if(input$interactionBinary=='Yes' && input$polyBinary=='Yes'&& input$splineBinary=='Yes'){
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"), paste("+poly(", input$polyOptions,",2)"))),data=indiaData)
      cv.lm(data = indiaData, form.lm = formula(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"), paste("+ns(", input$splineOptions,",",input$splineOptions1,")"), paste("+poly(", input$polyOptions,",2)")))), m=5, dots = 
              FALSE, seed=29, plotit=FALSE, printit=TRUE)
    }
    else
    {
      #lm(as.formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"))),data=indiaData)
      cv.lm(data = indiaData, form.lm = formula(paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"))), m=5)
    }
  })


  observeEvent(input$lmodel,{

    output$regTab <- renderPrint({

        summary(runRegression())

    })

    output$regPlot <- renderPlot({
      diagPlots<-runRegression()
      layout(matrix(c(1,2,3,4),2,2,byrow=T))
      plot(diagPlots)
    })
  }  )
  
  observeEvent(input$lmodelCv,{
    
    output$regTab <- renderPrint({
      
      runRegressionCv()
      
    })
    
    
  }  )

    runRandomForest <- reactive({
      #print((paste(input$depVariable," ~ ",paste(input$indepVariable,collapse="+"))))
      randomForest(as.formula(paste(input$depVariableRf," ~ ",paste(input$indepVariableRf,collapse="+"))),data=indiaData,na.action=na.roughfix,importance=TRUE)
    })



  observeEvent(input$rfmodel,{

    output$rfTab <- renderPrint({

      rfResults<-runRandomForest()
      rfResults
      #varImpPlot(rfResults)
    })
    output$rfPlot <- renderPlot({

      rfResults<-runRandomForest()
      varImpPlot(rfResults)
    })
    output$rfPlotEr <- renderPlot({
      
      rfResults<-runRandomForest()
      plot(rfResults)
    })
})
  
  lassoModel <- reactive({
    n <- nrow(indiaData)
    trainingSize <- ceiling(n*0.7)
    set.seed(37)
    train <- sample(1:n,trainingSize)
    indiaTrain <- indiaData[ train,]
    indiaTest  <- indiaData[-train,]
    xTrain <- as.matrix(indiaTrain[, names(indiaTrain) %in% c(input$indepVariableLasso)])
    yTrain <- as.matrix(indiaTrain[, input$depVariableLasso])
    glmnet(xTrain,yTrain, alpha=1)
  })



  observeEvent(input$lassoModel,{

    output$lassoTab <- renderPrint({
      lassoResults<-lassoModel()
      coef(lassoResults)
      #varImpPlot(rfResults)
    })

    output$lassoPlot <- renderPlot({

      lassoResults<-lassoModel()
      plot(lassoResults)
    })


  })
  
  lassoModelCv <- reactive({
    n <- nrow(indiaData)
    trainingSize <- ceiling(n*0.75)
    set.seed(37)
    train <- sample(1:n,trainingSize)
    indiaTrain <- indiaData[ trainingSize,]
    indiaTest  <- indiaData[-trainingSize,]
    x <- as.matrix(indiaTest[, names(indiaTest) %in% c(input$indepVariableLasso)])
    y <- as.matrix(indiaTest[, input$depVariableLasso])
    cv.glmnet(x,y, alpha=1)
  })
  
  
  observeEvent(input$lassoModelCv,{
    
    output$lassoTabCv <- renderPrint({
      lassoResults<-lassoModelCv()
      coef(lassoResults)
     
    })
    output$lassoTabCv1 <- renderPrint({
      lassoResults<-lassoModelCv()
      cbind(lambda=round(lassoResults$lambda,1),
            cvm=signif(lassoResults$cvm,7),
            cvsd=signif(lassoResults$cvsd,5))
      #varImpPlot(rfResults)
    })
    output$lassoTabCv2 <- renderPrint({
      lassoResults<-lassoModelCv()

      lambda.lse<-lassoResults$lambda.1se
      lambda.min<-lassoResults$lambda.min
      cbind(lambda.lse,lambda.min)
      #varImpPlot(rfResults)
    })
    
    output$lassoPlotCv <- renderPlot({
      
      lassoResultsCv<-lassoModelCv()
      plot(lassoResultsCv)
    })
    
    
  })



})
