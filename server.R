source('helpers.R')

# Uncomment ONLY when using on desktop!!!
# packages <- function(x) {
#   x <- as.character(match.call()[[2]])
#   if (!require(x, character.only = T)) {
#     install.packages(pkgs = x, repos = "http://cran.r-project.org")
#     require(x, character.only = T)
#   }
# }
# packages(shiny)
# packages(pracma) # library for the function nthroot
# packages(dplyr)
# packages(plotly)
# packages(DT)

library(shiny)
library(pracma) # library for the function nthroot
library(dplyr)
require(plotly)
library(DT)

shinyServer(function(input, output, session) {

  #prawdopodobienstwo wykrycia
  probabilityF <- function(){
    sampleN1 <- seq(input$mmNumber[1], input$mmNumber[2])
    probabilityC <- 1-(1-input$infR*input$czTp)^sampleN1
    probabilityDF <- as.data.frame(cbind('p'= probabilityC, 'N' = sampleN1))
    tablePR <- DT::datatable(data.frame('N' = sampleN1,
                                        'p'= round(probabilityC, digits = 3)),
                             class = 'table-condensed stripe hover order-column',
                             rownames = F, style = 'bootstrap', selection = 'none',
                             options = list(autoWidth = T, columnDefs =
                                              list(list(className = 'dt-center',
                                                        targets = 0:1),
                                                   list(className = 'dt', width = '200px',
                                                        targets = '_all'))))
    list(tabP = tablePR, prDF = probabilityDF)
  }
  
  #minimal sample size for detection of given infection rate
  #with given probability
  smpF <- function(){
   infRa2 <- seq(input$mmInf[1], input$mmInf[2], 0.0005)
   sampleN2 <- (log(1-input$probaB)/log(1-infRa2*input$czTn))
   saN2 <- as.data.frame(cbind('N' = sampleN2, 'infections' = infRa2))
   tableSMP <- DT::datatable(data.frame('Infections rate' = infRa2,
                                        'N' = ceiling(sampleN2)),
                             class = 'table-condensed stripe hover dt[-head|-body]-left order-column',
                             rownames = F, style = 'bootstrap', selection = 'none',
                             options = list(columnDefs =
                                              list(list(className = 'dt-center',
                                                        targets = 0:1),
                                                   list(className = 'dt', width = '200px',
                                                        targets = '_all'))))
   list(tabN = tableSMP, saN = saN2)
   }
  
  #infection rate detected with given sample size
  #and with given probability of detection
  rateiF <- function() {
  samN3 <- seq(1, input$sampleN3)
  infRa3 <- ((1-(nthroot((1-input$probaB3), samN3)))*1000)/input$czTg
  iR3 <- as.data.frame(cbind('Max.N.Infections' = infRa3, 'N' = samN3))
  tableG <- DT::datatable(data.frame('N' = samN3,
                                     'Upper bound of infections' = floor(infRa3)),
                          class = 'table-condensed stripe hover order-column',
                          rownames = F, style = 'bootstrap', selection = 'none',
                          options = list(columnDefs =
                                           list(list(className = 'dt-center',
                                                     targets = 0:1),
                                                list(className = 'dt', width = '200px',
                                                     targets = '_all'))))
  list(tabG = tableG, iR = iR3)
  }
  #plots----
  output$wykresP <- renderPlotly({
      plotDFP <- data.frame(probabilityF()$prDF)
      hovertxtp <- paste("p: ", round(plotDFP$p, digits = 2), "<br>",
                         "N: ", plotDFP$N)
      theGraphP <- plotDFP %>% plot_ly(x = ~N,
                                       hoverinfo = 'text', text = hovertxtp) %>%
                               add_lines(y = ~p, color = I("#e74b47")) %>%          
                               layout(xaxis = list(title = 'Sample size'),
                               yaxis = list(title = 'Probability of detection',
                                            tickangle = -30))
  })
  
  output$wykresN <- renderPlotly({
      plotDFN <- data.frame(smpF()$saN)
      hovertxtn <- paste("N: ", ceiling(plotDFN$N), "<br>",
                         "infections: ", round(plotDFN$infections, digits = 4))
      theGraphN <- plotDFN %>% plot_ly(x = ~infections,
                                       hoverinfo = 'text', text = hovertxtn) %>% 
                               add_lines(y=~N, color = I("#e74b47")) %>%          
                               layout(xaxis = list(title = 'Infections rate'),
                                      yaxis = list(title = 'Sample size',
                                                   tickangle = -30))
      })
  
  output$wykresG <- renderPlotly({
      
      plotDFG <- data.frame(rateiF()$iR)
      hovertxtg <- paste("infections: ", round(plotDFG$Max.N.Infections, digits = 0),
                      "<br>", "N: ", ceiling(plotDFG$N))
      theGraphG <- plotDFG %>% plot_ly(x = ~N,
                                       hoverinfo = 'text', text = hovertxtg) %>% 
                               add_lines(y = ~Max.N.Infections, color = I('#e74b47')) %>%
                               layout(xaxis = list(title = 'Sample size'),
                                      yaxis = list(title = 'Upper bound of infections (per 1000)',
                                                   tickangle = -30))
  })
  
  #tables----
  output$tabelaP <- DT::renderDataTable ({
    probabilityF()$tabP
    })
  
  output$tabelaN <- DT::renderDataTable ({
    smpF()$tabN
    })
  
  output$tabelaG <- DT::renderDataTable ({
    rateiF()$tabG
  })
})
