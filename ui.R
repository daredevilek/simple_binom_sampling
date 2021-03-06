source('helpers.R')
library(shiny)
library(pracma) # library for the function nthroot
library(dplyr)
require(plotly)
library(DT)

shinyUI(
  navbarPage(
    'Sampling and probability',
    theme = 'bootstrap.css', inverse = T,
    #first tab----
    tabPanel(
      'Probability of detection',
      sidebarLayout(
        position = 'left',
        sidebarPanel(
          #slider sensitivity----
          sliderInput(
            inputId = 'testSensitivity',
            label = 'Sensitivity of a test',
            min = 0.60, max = 1.00, value = 0.90,step = 0.05
            ),
          #slider inf rate----
          sliderInput(
            inputId = "infR",
            label = "Infection rate",
            min = 0.000, max = 0.0500, value = 0.0030, step = 0.0005, sep = ''
            ),
          #slider n proby----
          sliderInput(
            inputId = 'mmNumber',
            label = 'Sample size (min - max)',
            min = 25, max = 3600, step = 25, value = c(200, 1000)
            ),
          br(),
          footer),
          #main panel----
          mainPanel(tabsetPanel(
            tabPanel(
              title = "Plot", 
              plotlyOutput(outputId = 'plotP', height = '600px')),
            tabPanel(
              title = 'Table', DT::dataTableOutput('tableP')
              )))
        )),
    #second tab----
    tabPanel(
      'Min. sample size',
      sidebarLayout(
        position = 'left',
        sidebarPanel(
          #slider sensitivity----
          sliderInput(
            inputId = 'testSensitivityN',
            label = 'Sensitivity of a test',
            min = 0.60, max = 1.00, value = 0.90,step = 0.05
            ),
          #slider p detection----
          sliderInput(
            inputId = 'probaB',
            label = 'Probability of detection',
            min = 0.01, max = 1.00, value = 0.75, step = 0.01
            ),
          #slider p inf----
          sliderInput(
            inputId = 'mmInf',
            label = 'Infection rate (min - max)',
            min = 0.000, max = 0.050, value = c(0.001, 0.030), step = 0.0005, sep = ''
            ),
          br(),
          footer),
          #main panel----
          mainPanel(tabsetPanel(
            tabPanel(
              title = "Plot", 
              plotlyOutput(outputId = 'plotN', height = '600px')),
            tabPanel(
              title = 'Table', DT::dataTableOutput('tableN')
              )))
        )),
    #third tab----
    tabPanel(
      'Upper boundary of infection rate', 
      sidebarLayout(
        position = 'left',
        sidebarPanel(
          #slider sensitivity----
          sliderInput(
            inputId = 'testSensitivityG',
            label = 'Sensitivity of a test',
            min = 0.60, max = 1.00, value = 0.90,step = 0.05
          ),
          #Slider ProbaB3----
          sliderInput(
            inputId = "probaB3",
            label = "Probability of detection",
            min = 0.500, max = 1.000, value = 0.800, step = 0.005
          ),
          #slider SampleN3----
          sliderInput(
            inputId = 'sampleN3',
            label = 'Sample size',
            val = 800, min = 0, max = 4000, step = 25
        ),
        br(),
        footer),
        #main panel----
      mainPanel(tabsetPanel(
        tabPanel(
          title = "Plot", 
          plotlyOutput(outputId = 'plotG', height = '600px')),
        tabPanel(
          title = 'Table', DT::dataTableOutput('tableG')
        )))
    ))))
    #footer----
    #tags$div(class = 'panel-footer', style = 
    #            'position: fixed; display: block;
    #            bottom: 5px; width = 100%',  
    #            tags$p('Aplikacja zbudowana w', a('Shiny', href = 'http://www.rstudio.com/shiny'),
    #              'dla', a('R Studio', href = 'http://www.rstudio.com') , 'na podstawie',
    #              a('kodu Renke Luekhen.', href = 'https://goo.gl/FopD9R')
    #            ))))
    
    
    