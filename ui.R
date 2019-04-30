#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(plotly)

colnamesIndia<-colnames(indiaData)
mapTypes<-c('map','mapmedian','maptail','mapcum')
plotTypes<-c('dot','arrow','bar','normbar','scatdot')
ui <- dashboardPage(skin = "blue",
  dashboardHeader(
    title = "Economy and Health Indicators of India",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Models", icon = icon("chart-line"), tabName = "model")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              tags$h3("Scatter Plot"),
  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      selectInput("X", "X",
                  colnamesIndia,
                  selected = "Infant Mortality Rate"),
      selectInput("Y", "Y",
                  colnamesIndia,
                  selected = "Institutional Births"),
      selectInput("Classification", "Classification",
                  c("Region"="Region",
                   "State_Size"="State.Size.Classification"
                  ),
                  selected = "Institutional Births")),


    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot", height = "400px", width = "600px")

    )
    ),tags$h3("Linked Micromap"),
  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      selectInput("col3","Column-3",
                  colnamesIndia,
                  selected = "Infant Mortality Rate"),
      selectInput("plotTypeCol3","Plot type for column-3",
                  plotTypes,
                  selected = "dot"),
      selectInput("col4", "Column-4",
                  colnamesIndia,
                  selected = "Institutional Births"),
      selectInput("plotTypeCol4","Plot type for column-4",
                  plotTypes,
                  selected = "dot"),
      selectInput("mapTypes", "Map Type",
                  mapTypes,

                  selected = "Institutional Births"),
      selectInput("sorVar", "Choose the sort variable",
                  colnamesIndia,
                  selected = "Institutional Births"),

      actionButton(inputId ="go", "Go")
    ),

    mainPanel(

      uiOutput('fetchPDF')

    )
  )),
  tabItem(tabName = "model",
          tags$h3("Linear Regression"),

  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      selectInput("depVariable","Dependent Variable",
                  colnamesIndia,
                  selected = "Infant Mortality Rate"),
      selectInput("indepVariable","Independent Variables",
                  colnamesIndia,
                  selected = "IMR",multiple = TRUE),
      radioButtons(inputId="interactionBinary", label="Interaction term", choices=c("Yes","No"), selected = "No", inline = TRUE,
                   width = NULL),
      uiOutput('fetchInteractOptions'),
      radioButtons(inputId="polyBinary", label="Polynomial term", choices=c("Yes","No"), selected = "No", inline = TRUE,
                   width = NULL),
      uiOutput('fetchPolyOptions'),
      radioButtons(inputId="splineBinary", label="Spline term", choices=c("Yes","No"), selected = "No", inline = TRUE,
                  width = NULL),
      uiOutput('fetchSplineOptions'),
      uiOutput('fetchSplineOptions1'),
      actionButton(inputId ="lmodel", "Create the model!"),
      actionButton(inputId ="lmodelCv", "Cross Validate")
    ),

    mainPanel(
      verbatimTextOutput("regTab"),
      plotOutput("regPlot")
    )
  ),
  tags$h3("Random Forest"),
  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      selectInput("depVariableRf","Dependent Variable",
                  colnamesIndia,
                  selected = "Infant Mortality Rate"),
      selectInput("indepVariableRf","Independent Variables",
                  colnamesIndia,
                  selected = "IMR",multiple = TRUE),
      actionButton(inputId ="rfmodel", "Create the model!")
      
    ),

    mainPanel(
      verbatimTextOutput("rfTab"),
      plotOutput("rfPlot", height = "400px", width = "600px"),
      plotOutput("rfPlotEr", height = "400px", width = "600px"),
      verbatimTextOutput("rfTabCv")
    )
  ),
  tags$h3("Lasso Regression"),
  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      selectInput("depVariableLasso","Dependent Variable",
                  colnamesIndia,
                  selected = "Infant Mortality Rate"),
      selectInput("indepVariableLasso","Independent Variables",
                  colnamesIndia,
                  selected = "IMR",multiple = TRUE),

      actionButton(inputId ="lassoModel", "Create the model!"),
      actionButton(inputId ="lassoModelCv", "Cross Validate!")
    ),

    mainPanel(
      verbatimTextOutput("lassoTab"),
      plotOutput("lassoPlot", height = "400px", width = "600px"),
      verbatimTextOutput("lassoTabCv"),
      verbatimTextOutput("lassoTabCv1"),
      verbatimTextOutput("lassoTabCv2"),
      plotOutput("lassoPlotCv", height = "400px", width = "600px")

    )
  )

  ))
))
