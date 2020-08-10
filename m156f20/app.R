###########################################################################
#
#       1. highlight all code in this file (ie: use Ctrl A)
#       2. run highlighted code (ie: use Ctrl Enter or click the green arrow)
#       3. allow R to install any missing packages (ie: click yes)
#       4. if packages needed to be installed, highlight and run again
#
##################################################################


list.of.packages <- c("dplyr", "shiny", "knitr", "ggplot2", "plotly", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(knitr)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)


types <- c("Barchart","Histogram", "Boxplot", "Scatterplot")
stypes <- c("Numeric Summary", "Frequencies", "Correlation and Regression")

ui <- fluidPage(
 tabsetPanel(
  tabPanel("Upload Data",
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      DTOutput("contents")
    )
  )),
  tabPanel("Visualize the Data",
           sidebarPanel(
             selectInput('gtype', "Graph Type", types),
             conditionalPanel(condition = 'input.gtype == "Barchart"',
                              uiOutput("invar_sel"),
                              uiOutput("outvar_sel"),
                              selectInput('bartype', "Chart Options",
                                          c("Simple", "Stack", "Cluster", 
                                            "Condition"))),
             conditionalPanel(condition = 'input.gtype == "Histogram"',
                              uiOutput("invar_selh")),
             conditionalPanel(condition = 'input.gtype == "Boxplot"',
                              uiOutput("invar_selb"),
                              uiOutput("outvar_selb")),
             conditionalPanel(condition = 'input.gtype == "Scatterplot"',
                              uiOutput("invar_sels"),
                              uiOutput("outvar_sels"),
                              uiOutput("colvar_sels"))
           ),
        mainPanel(plotlyOutput("graph"))
           ),
  tabPanel("Summarize the Data",
       sidebarPanel(selectInput('stype', "Descriptive Method", stypes),
                conditionalPanel(condition = 'input.stype == "Numeric Summary"',
                                           uiOutput("invar_sum"),
                                           uiOutput("outvar_sum")),
                conditionalPanel(condition = 'input.stype == "Frequencies"',
                                 uiOutput("invar_sump"),
                                 uiOutput("outvar_sump"),
                                 uiOutput("layervar_sump")),
                conditionalPanel(condition = 'input.stype == "Correlation and Regression"',
                                 uiOutput("invar_sumc"),
                                 uiOutput("outvar_sumc"))
           ),
       mainPanel(tableOutput("sumtab"),
                 conditionalPanel(condition = 'input.stype == "Correlation and Regression'),
                 plotlyOutput("corplot"))
  ))
)

server <- function(input, output) {
  data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })
  
  output$invar_sel <- renderUI(
    selectInput("invar", "Inner Variable", names(data()))
  )
  
  output$outvar_sel <- renderUI(
    selectInput("outvar", "Outer Variable (if applicable)", c("None", names(data())))
  )
  
  output$invar_selh <- renderUI(
    selectInput("invarh", "Variable", names(data())[sapply(data(), is.numeric)])
  )
  
  
  output$invar_selb <- renderUI(
    selectInput("invarb", "Numeric Variable", names(data())[sapply(data(), is.numeric)])
  )
  
  output$outvar_selb <- renderUI(
    selectInput("outvarb", "Categorical Variable (if applicable)", c("None", names(data())))
  )

  output$invar_sels <- renderUI(
    selectInput("invars", "X Variable", names(data())[sapply(data(), is.numeric)])
  )
  output$outvar_sels <- renderUI(
    selectInput("outvars", "Y Variable", names(data())[sapply(data(), is.numeric)])
  )
  output$colvar_sels <- renderUI(
    selectInput("colvars", "Color Variable", c("None",names(data())))
  )
  
  output$invar_sum <- renderUI(
    selectInput("invar_sum", "Numeric Variable", names(data())[sapply(data(), is.numeric)])
  )
  output$outvar_sum <- renderUI(
    selectInput("outvar_sum", "Grouping Variable (if desired)", c("None",names(data())))
  )
  
  output$invar_sump <- renderUI(
    selectInput("invar_sump", "Variable 1", names(data()))
  )
  output$outvar_sump <- renderUI(
    selectInput("outvar_sump", "Variable 2 (if desired)", c("None",names(data())))
  )
  output$layervar_sump <- renderUI(
    selectInput("layervar_sump", "Layer Variable (if desired)", c("None",names(data())))
  )
  output$invar_sumc <- renderUI(
    selectInput("invar_sumc", "X Variable", names(data())[sapply(data(), is.numeric)])
  )
  output$outvar_sumc <- renderUI(
    selectInput("outvar_sumc", "Y Variable", names(data())[sapply(data(), is.numeric)])
  )
  
  output$contents <- renderDT({
    if(is.null(data())){return ()}
    data()
  })
  
output$graph <- renderPlotly({
  if(input$gtype == "Barchart"){
    if(input$bartype == "Simple"){
      gp <- ggplot(data = data(), aes(x = get(input$invar))) + 
            geom_bar(stat = "count") + labs(x = input$invar)   + theme_minimal()
      ggplotly(p = gp, tooltip = "y")
    } else if(input$bartype == "Stack"){
      gp <- ggplot(data = data(), aes(x = get(input$invar),
                                      fill = get(input$outvar))) + 
        geom_bar(stat = "count") + 
        labs(x = input$invar, fill = input$outvar)   + theme_minimal()
      ggplotly(p = gp, tooltip = "y")
    } else if(input$bartype == "Cluster"){
      gp <- ggplot(data = data(), aes(x = get(input$invar),
                                      fill = get(input$outvar))) + 
        geom_bar(stat = "count", position = "dodge") + 
        labs(x = input$invar, fill = input$outvar)   + theme_minimal()
      ggplotly(p = gp, tooltip = "y")
    } else if(input$bartype == "Condition"){
      df <- group_by(data(), get(input$invar), get(input$outvar)) %>% summarize(value = n())
      df <- data.frame(df)
      names(df) <- c(input$invar, input$outvar, "value")
      gp <- ggplot(df, aes(x = get(input$invar), y = value,
                           fill = get(input$outvar))) + 
        geom_bar(position = "fill", stat = "identity") + 
        labs(x = input$invar, fill = input$outvar)   + theme_minimal()
      ggplotly(p = gp, tooltip = "y")
    }
    } else if(input$gtype == "Histogram"){
      gp <- ggplot(data = data(), aes(x = get(input$invarh))) + 
        geom_histogram()  + theme_minimal() + labs(x = input$invarh)
      ggplotly(p = gp, tooltip = "y")
    } else if(input$gtype == "Boxplot"){
      if(input$outvarb == "None"){
      gp <- ggplot(data = data(), aes(y = get(input$invarb))) + 
        geom_boxplot() + labs(y = input$invarb)  + theme_minimal()
      ggplotly(p = gp)
      } else {
        gp <- ggplot(data = data(), aes(y = get(input$invarb), x = get(input$outvarb))) + 
          geom_boxplot() + labs(y = input$invarb, x = input$outvarb)   + theme_minimal()
        ggplotly(p = gp)
      } 
    } else if (input$gtype == "Scatterplot"){
      if(input$colvars == "None"){
      gp <- ggplot(data = data(), aes(x = get(input$invars), y = get(input$outvars),
                                      text = paste0("X = ",get(input$invars),", Y = ",get(input$outvars)))) + 
        geom_point() + labs(x = input$invars, y = input$outvars)   + theme_minimal()
      ggplotly(p = gp, tooltip = "text")
      } else{
        gp <- ggplot(data = data(), aes(x = get(input$invars), 
                                        y = get(input$outvars),
                                        color = get(input$colvars),
                                        text = paste0("X = ",get(input$invars),", Y = ",get(input$outvars),
                                                      ", ", input$colvars, " = ", get(input$colvars)))) + 
          geom_point() + labs(x = input$invars, y = input$outvars, color = input$colvars)   + theme_minimal()
        ggplotly(p = gp, tooltip = "text")
      }
      }
})  
  

output$sumtab <- renderTable({
  if(input$stype == "Numeric Summary"){
    if(input$outvar_sum == "None"){
      summarize(data(), Mean = mean(get(input$invar_sum)),
                        StDev = sd(get(input$invar_sum)),
                        N = length(get(input$invar_sum)),
                        Min = min(get(input$invar_sum)),
                        Q1 = quantile(get(input$invar_sum), probs = .25),
                        Median = median(get(input$invar_sum)),
                        Q3 = quantile(get(input$invar_sum), probs = .75),
                        Max = max(get(input$invar_sum))
                )
    } else {
      df1 <- group_by(data(), get(input$outvar_sum)) %>%
      summarize(Mean = mean(get(input$invar_sum)),
                StDev = sd(get(input$invar_sum)),
                N = length(get(input$invar_sum)),
                Min = min(get(input$invar_sum)),
                Q1 = quantile(get(input$invar_sum), probs = .25),
                Median = median(get(input$invar_sum)),
                Q3 = quantile(get(input$invar_sum), probs = .75),
                Max = max(get(input$invar_sum))
      )
      names(df1)[1] <- input$outvar_sum
      df1
    }
  } else if (input$stype == "Frequencies"){
    if(input$outvar_sump == "None"){
      with(data(), table(get(input$invar_sump)))
    } else if (input$layervar_sump == "None") {
     with(data(), table(get(input$invar_sump), get(input$outvar_sump)))
    } else {
      with(data(), table(get(input$invar_sump), get(input$outvar_sump),get(input$layervar_sump)))  
      }
      } else if(input$stype == "Correlation and Regression"){
        cr <- with(data(),cor(x = get(input$invar_sumc), y = get(input$outvar_sumc)))
        lmr <- with(data(), lm(get(input$outvar_sumc) ~ get(input$invar_sumc)))
        round(data.frame(Correlation = cr, Intercept = coef(lmr)[1], Slope = coef(lmr)[2]), 2)
      }
})

output$corplot <- renderPlotly({
  if(input$stype == "Correlation and Regression"){
  ggp <- ggplot(data(), aes(x = get(input$invar_sumc), y = get(input$outvar_sumc),
                            text = paste0("X = ",get(input$invar_sumc),", Y = ",get(input$outvar_sumc)))) +
         geom_point() + geom_smooth(method = "lm", se = FALSE) +
         labs(x = input$invar_sumc, y = input$outvar_sumc)
  ggplotly(ggp, tooltip = "text")
  } else {
    plotly_empty()
  }
  
})

}

shinyApp(ui, server)

