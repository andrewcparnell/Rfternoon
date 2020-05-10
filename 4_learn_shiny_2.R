# A second shiny example

library(shiny)
library(ggplot2)
library(plotly)

explore = function(data) {
  ggplot2::theme_set(ggplot2::theme_minimal())
  
  shiny::shinyApp(
    ui = navbarPage(
      "Data Explorer",
      tabPanel("Raw data",
               DT::dataTableOutput("table")),
      navbarMenu(
        "Summary",
        tabPanel("1 variable",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("selVar1DSummary", "Select variable",
                                 choices = colnames(data)),
                     checkboxGroupInput(
                       'summStat',
                       label = 'Summary statistics',
                       choices = list(
                         "Mean" = 'mean',
                         "Median" = 'median',
                         "Standard deviation" = 'sd',
                         "Summary" = 'summary'
                       ),
                       selected = c('mean', 'median', 'sd', 'summary')
                     )
                   ),
                   mainPanel(verbatimTextOutput("OneDsummary"))
                 )),
        tabPanel("2 variables",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "selVar2DSummary1",
                       "Select variable/category 1",
                       choices = colnames(data)
                     ),
                     selectInput("selVar2DSummary2", "Select variable 2",
                                 choices = colnames(data)),
                     checkboxGroupInput(
                       'TwoDStat',
                       label = 'Summary statistics',
                       choices = list(
                         "Table" = 'table',
                         "Correlation" = 'cor',
                         "Aggregate" = 'agg'
                       ),
                       selected = c('table')
                     ),
                     uiOutput("ui2Dsummary")
                   ),
                   mainPanel(verbatimTextOutput("TwoDsummary"))
                 ))
      ),
      navbarMenu(
        "Plots",
        tabPanel("1 variable",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("selVar", "Select variable",
                                 choices = colnames(data)),
                     radioButtons(
                       "plotType1D",
                       "Plot type",
                       c(
                         "Histogram" = "hist",
                         "Kernel Density" = "dens",
                         "Bar chart" = "bar"
                       )
                     ),
                     uiOutput("uiXSlider"),
                     uiOutput("ui1D")
                   ),
                   mainPanel(plotOutput("OneDplot"))
                 )),
        tabPanel("2 variable",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "selVar1",
                       "Horizontal axis variable/category 1",
                       choices = colnames(data)
                     ),
                     selectInput("selVar2", "Vertical axis variable 2",
                                 choices = colnames(data)),
                     radioButtons(
                       "plotType2D",
                       "Plot type",
                       c("Scatter" = "scat",
                         "Box plot" = "box")
                     ),
                     uiOutput("uiXSlider2D"),
                     uiOutput("uiYSlider2D"),
                     uiOutput("ui2D")
                   ),
                   mainPanel(plotlyOutput("TwoDplot"))
                 ))
      )
    )
    ,
    server = function(input, output) {
      output$OneDsummary <- renderPrint({
        cat('Statistics for', input$selVar1DSummary, '\n\n')
        if ('mean' %in% input$summStat) {
          cat('mean:', mean(data[, input$selVar1DSummary]), '\n\n')
        }
        if ('median' %in% input$summStat) {
          cat('median:', median(data[, input$selVar1DSummary]), '\n\n')
        }
        if ('sd' %in% input$summStat) {
          cat('sd:', sd(data[, input$selVar1DSummary]), '\n\n')
        }
        if ('summary' %in% input$summStat) {
          cat('summary:\n')
          print(summary(data[, input$selVar1DSummary]))
        }
      })
      
      output$TwoDsummary <- renderPrint({
        if ('table' %in% input$TwoDStat) {
          cat('Table of',
              input$selVar2DSummary1,
              'vs',
              input$selVar2DSummary2,
              '\n')
          print(table(data[, input$selVar2DSummary1],
                      data[, input$selVar2DSummary2]))
        }
        if ('cor' %in% input$TwoDStat) {
          cat(
            'Correlation between',
            input$selVar2DSummary1,
            'and',
            input$selVar2DSummary2,
            '\n'
          )
          print(cor(data[, input$selVar2DSummary1],
                    data[, input$selVar2DSummary2]))
        }
        if ('agg' %in% input$TwoDStat) {
          print(aggregate(data[, input$selVar2DSummary2],
                          by = list(data[, input$selVar2DSummary1]),
                          input$fun))
        }
      })
      
      output$OneDplot <- renderPlot({
        limX = input$histSlider
        if (input$plotType1D == 'hist') {
          ggplot(data, aes_string(x = input$selVar,
                                  fill = '..count..')) +
            geom_histogram(bins = input$nBins) +
            lims(x = limX) +
            theme(legend.position = 'None')
          # hist(data[,input$selVar], breaks = input$nBins)
        } else if (input$plotType1D == 'dens') {
          ggplot(data, aes_string(x = input$selVar)) +
            geom_density(bw = input$bw,
                         fill = "blue",
                         alpha = 0.5) +
            lims(x = limX) +
            theme(legend.position = 'None')
          # plot(density(data[,input$selVar], bw = input$bw))
        } else if (input$plotType1D == 'bar') {
          ggplot(data, aes_string(x = input$selVar,
                                  fill = input$selVar)) +
            geom_bar() +
            theme(legend.position = 'None')
        }
      })
      
      output$TwoDplot <- renderPlotly({
        limX = input$sliderX2D
        limY = input$sliderY2D
        if (input$plotType2D == 'scat') {
          p = ggplot(data,
                     aes_string(
                       x = input$selVar1,
                       y = input$selVar2,
                       tooltip = 1:nrow(data)
                     )) +
            geom_point() +
            lims(x = limX, y = limY)
          ggplotly(p)
        } else if (input$plotType2D == 'box') {
          x = as.factor(get(input$selVar1, pos = data))
          y = get(input$selVar2, pos = data)
          z = 1:nrow(data)
          p2 = ggplot(data,
                      aes(x = x, y = y,
                          fill = x
                      )) +
            geom_boxplot() +
            lims(y = limY) +
            theme(legend.position = 'None')
          ggplotly(p2)
        }
      })
      
      output$uiXSlider <- renderUI({
        x = get(input$selVar, pos = data)
        prettyX = pretty(x, 10)
        if(input$plotType1D != 'box') {
          sliderInput("histSlider",
                      label = "horizontal axis range",
                      min = min(prettyX),
                      max = max(prettyX),
                      value = c(min(prettyX),max(prettyX)))
        }
      })
      
      output$uiXSlider2D <- renderUI({
        x = get(input$selVar1, pos = data)
        prettyX = pretty(x, 10)
        if(input$plotType2D == 'scat') {
          sliderInput("sliderX2D",
                      label = "horizontal axis range",
                      min = min(prettyX),
                      max = max(prettyX),
                      value = c(min(prettyX),max(prettyX)))
        }
      })
      
      output$uiYSlider2D <- renderUI({
        y = get(input$selVar2, pos = data)
        prettyY = pretty(y, 10)
        sliderInput("sliderY2D",
                    label = "vertical axis range",
                    min = min(prettyY),
                    max = max(prettyY),
                    value = c(min(prettyY),max(prettyY)))
      })
      
      output$ui1D <- renderUI({
        switch(
          input$plotType1D,
          "hist" = sliderInput(
            "nBins",
            "Number of bins",
            min = 10,
            max = 50,
            value = 30
          ),
          "dens" = sliderInput(
            "bw",
            "Bandwidth",
            min = 0,
            max = ceiling(2 * bw.nrd0(data[, input$selVar])),
            value = bw.nrd0(data[, input$selVar])
          )
        )
      })
      
      output$ui2Dsummary <- renderUI({
        if ('agg' %in% input$TwoDStat)
          textInput("fun",
                    label = HTML("Function to aggregate by<br/>(e.g. mean, sd, sum, ...)"),
                    value = "mean")
      })
      
      output$table <- DT::renderDataTable(DT::datatable({
        data
      },
      filter = 'top',
      options = list(
        pageLength = 25,
        searchHighlight = TRUE,
        autoWidth = TRUE,
        class = 'cell-border stripe'
      )))
    }
  )
}


# Run the function
explore(iris)


# What else can I do? -----------------------------------------------------

# The shiny gallery: https://shiny.rstudio.com/gallery/
# In particular look at the different widgets: https://shiny.rstudio.com/gallery/widget-gallery.html
# Notet hat you can get the code of all these
# Share your own shiny apps: https://www.shinyapps.io
# Shiny cheat sheet https://shiny.rstudio.com/articles/cheatsheet.html
# Mastering shiny book: https://mastering-shiny.org

# Exercise ----------------------------------------------------------------

# Pick a widget from the widget gallery above and try to implement it in the two shiny apps (start with the simple one). 
