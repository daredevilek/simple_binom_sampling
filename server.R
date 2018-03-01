source('helpers.R')
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
  
  #minimalna wielkosc proby do wykrycia danego poziomu infekcyjnosci
  #z danym prawdopodobienstwem
  smpF <- function(){
   infRa2 <- seq(input$mmInf[1], input$mmInf[2], 0.0005)
   sampleN2 <- (log(1-input$probaB)/log(1-infRa2*input$czTn))
   saN2 <- as.data.frame(cbind('N' = sampleN2, 'infekcje' = infRa2))
   tableSMP <- DT::datatable(data.frame('Częstość infekcji' = infRa2,
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
  
  #poziom inekcyjnosci wykrywany przy danej liczebosci proby
  #i danym prawdopodobienstwie wykrycia
  rateiF <- function() {
  samN3 <- seq(1, input$sampleN3)
  infRa3 <- ((1-(nthroot((1-input$probaB3), samN3)))*1000)/input$czTg
  iR3 <- as.data.frame(cbind('Maks.L.Infekcji' = infRa3, 'N' = samN3))
  tableG <- DT::datatable(data.frame('N' = samN3,
                                     'Graniczny poziom infekcji' = floor(infRa3)),
                          class = 'table-condensed stripe hover order-column',
                          rownames = F, style = 'bootstrap', selection = 'none',
                          options = list(columnDefs =
                                           list(list(className = 'dt-center',
                                                     targets = 0:1),
                                                list(className = 'dt', width = '200px',
                                                     targets = '_all'))))
  list(tabG = tableG, iR = iR3)
  }
  #wykresy----
  output$wykresP <- renderPlotly({
      plotDFP <- data.frame(probabilityF()$prDF)
      hovertxtp <- paste("p: ", round(plotDFP$p, digits = 2), "<br>",
                         "N: ", plotDFP$N)
      theGraphP <- plotDFP %>% plot_ly(x = ~N,
                                       hoverinfo = 'text', text = hovertxtp) %>%
                               add_lines(y = ~p, color = I("#e74b47")) %>%          
                               layout(xaxis = list(title = 'Liczebność próby'),
                               yaxis = list(title = 'Prawdopodobieństwo wykrycia',
                                            tickangle = -30))
  })
  
  output$wykresN <- renderPlotly({
      plotDFN <- data.frame(smpF()$saN)
      hovertxtn <- paste("N: ", ceiling(plotDFN$N), "<br>",
                         "infekcje: ", round(plotDFN$infekcje, digits = 4))
      theGraphN <- plotDFN %>% plot_ly(x = ~infekcje,
                                       hoverinfo = 'text', text = hovertxtn) %>% 
                               add_lines(y=~N, color = I("#e74b47")) %>%          
                               layout(xaxis = list(title = 'Poziom infekcji'),
                                      yaxis = list(title = 'Liczebność próby',
                                                   tickangle = -30))
      })
  
  output$wykresG <- renderPlotly({
      
      plotDFG <- data.frame(rateiF()$iR)
      hovertxtg <- paste("infekcje: ", round(plotDFG$Maks.L.Infekcji, digits = 0),
                      "<br>", "N: ", ceiling(plotDFG$N))
      theGraphG <- plotDFG %>% plot_ly(x = ~N,
                                       hoverinfo = 'text', text = hovertxtg) %>% 
                               add_lines(y = ~Maks.L.Infekcji, color = I('#e74b47')) %>%
                               layout(xaxis = list(title = 'Liczebność próby'),
                                      yaxis = list(title = 'Graniczny poziom infekcji (na 1000)',
                                                   tickangle = -30))
  })
  
  #tabele----
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

#opcjonalnie----

