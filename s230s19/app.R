library(shiny)
library(ggplot2)
library(viridis)
library(mgcv)
library(caret)

set.seed(123)
X1 <- runif(300,0,1)
X2 <- runif(300,0,1)
e <- rnorm(300,0,.5)
df <- expand.grid(X1 = seq(0,1,length.out = 100), X2 = seq(0,1,length.out = 100))

# Define UI ----
ui <- fluidPage(
  titlePanel("Understanding Variable Selection and Model Effects"),
  
  sidebarLayout(
    sidebarPanel(" ",
                 selectInput("mod", 
                             label = "Choose a type of Model",
                             choices = list("Linear Regression (no interaction)", 
                                            "Linear Regression (with interaction)",
                                            "Generalized Additive Model (GAM)",
                                            "K-Nearest Neighbors (k = 10)"),
                             selected = "Linear Regression (no interaction)"),
                 
                 selectInput("rel", 
                             label = "Choose how X1, X2, and Y are related",
                             choices = list("Linear (Y = X1 + error)", 
                                            "Linear (Y = X1 + X2 + error)", 
                                            "Linear with interaction (Y = X1 + X2 + X1*X2 + error)",
                                            "Quadratic in X2 (Y = X1 + X2 + X2^2 + error)", 
                                            "Sin (Y = sin(2pi*(X1 + X2) + error)"),
                             selected = "Linear (Y = X1 + X2 + error)"),
                 radioButtons("but",
                              label = "What to show?",
                              choices = list("Prediction Surface", "Residuals"))
    ),
    mainPanel(h2(" "),
              textOutput("selected_rel"),
              textOutput("selected_mod"),
              plotOutput("plot"),
              plotOutput("plot2"))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_mod <- renderText({ 
    paste("Modeled using:", input$mod)
  })
  
  output$selected_rel <- renderText({ 
    paste("True Relationship:", input$rel)
  })
  
  output$plot <- renderPlot({
    Y <- switch(input$rel,
                "Linear (Y = X1 + X2 + error)" = X1 + X2 + e, 
                "Linear (Y = X1 + error)" = X1 + e,
                "Linear with interaction (Y = X1 + X2 + X1*X2 + error)" = X1 + X2 + 1.5*X1*X2 + e,
                "Quadratic in X2 (Y = X1 + X2 + X2^2 + error)" = X1 + X2 + 1.5*X2^2 + e, 
                "Sin (Y = sin(2pi*(X1 + X2) + error)" = sin(2*3.14*(X1 + X2)) + e)
    
    
    predY <- switch(input$mod,
                    "Linear Regression (no interaction)" = predict(lm(Y ~ X1 + X2), newdata = df), 
                    "Linear Regression (with interaction)" = predict(lm(Y ~ X1 + X2 + X1*X2), newdata = df),
                    "Generalized Additive Model (GAM)" = predict(mgcv::gam(Y ~ s(X1) + s(X2)), newdata = data.frame(df), type = "response"),
                    "K-Nearest Neighbors (k = 10)" = predict(caret::knnreg(Y ~ X1 + X2, data = data.frame(Y = Y, X1 = X1, X2 = X2), k = 10), newdata = data.frame(df)))
    
    resid <- switch(input$mod,
                    "Linear Regression (no interaction)" = residuals(lm(Y ~ X1 + X2)), 
                    "Linear Regression (with interaction)" = residuals(lm(Y ~ X1 + X2 + X1*X2)),
                    "Generalized Additive Model (GAM)" = residuals(mgcv::gam(Y ~ s(X1) + s(X2))),
                    "K-Nearest Neighbors (k = 10)" = Y - predict(caret::knnreg(Y ~ X1 + X2, data = data.frame(Y = Y, X1 = X1, X2 = X2), k = 10)))
    
    resid_noX1 <- switch(input$mod,
                         "Linear Regression (no interaction)" = residuals(lm(Y ~ X2)), 
                         "Linear Regression (with interaction)" = residuals(lm(Y ~  X2)),
                         "Generalized Additive Model (GAM)" = residuals(mgcv::gam(Y ~  s(X2))),
                         "K-Nearest Neighbors (k = 10)" = Y - predict(caret::knnreg(Y ~ X2, data = data.frame(Y = Y, X1 = X1, X2 = X2), k = 10)))
    
    resid_noX2 <- switch(input$mod,
                         "Linear Regression (no interaction)" = residuals(lm(Y ~ X1)), 
                         "Linear Regression (with interaction)" = residuals(lm(Y ~  X1)),
                         "Generalized Additive Model (GAM)" = residuals(mgcv::gam(Y ~  s(X1))),
                         "K-Nearest Neighbors (k = 10)" = Y - predict(caret::knnreg(Y ~ X1, data = data.frame(Y = Y, X1 = X1, X2 = X2), k = 10)))
    
    
    predY_noX1 <- switch(input$mod,
                         "Linear Regression (no interaction)" = predict(lm(Y ~ X2), newdata = df), 
                         "Linear Regression (with interaction)" = predict(lm(Y ~ X2), newdata = df),
                         "Generalized Additive Model (GAM)" = predict(mgcv::gam(Y ~ s(X2)), newdata = data.frame(df), type = "response"),
                         "K-Nearest Neighbors (k = 10)" = predict(caret::knnreg(Y ~ X2, data = data.frame(Y = Y, X1 = X1, X2 = X2), k = 10), newdata = data.frame(df)))
    
    predY_noX2 <- switch(input$mod,
                         "Linear Regression (no interaction)" = predict(lm(Y ~ X1), newdata = df), 
                         "Linear Regression (with interaction)" = predict(lm(Y ~ X1), newdata = df),
                         "Generalized Additive Model (GAM)" = predict(mgcv::gam(Y ~ s(X1)), newdata = data.frame(df), type = "response"),
                         "K-Nearest Neighbors (k = 10)" = predict(caret::knnreg(Y ~ X1, data = data.frame(Y = Y, X1 = X1, X2 = X2), k = 10), newdata = data.frame(df)))
    
    
    trueY <- with(df, switch(input$rel,
                             "Linear (Y = X1 + X2 + error)" = X1 + X2, 
                             "Linear (Y = X1 + error)" = X1,
                             "Linear with interaction (Y = X1 + X2 + X1*X2 + error)" = X1 + X2 + 1.5*X1*X2,
                             "Quadratic in X2 (Y = X1 + X2 + X2^2 + error)" = X1 + X2 + 1.5*X2^2, 
                             "Sin (Y = sin(2pi*(X1 + X2) + error)" = sin(2*3.14*(X1 + X2))))
    
    ## Add components to data frame for surface plot
    df$trueY <- trueY
    df$predY <- predY
    df$predY_noX2 <- predY_noX2
    df$predY_noX1 <- predY_noX1
    
    ## Data frames for residual plots
    df2 <-data.frame(X1 = X1, X2 = X2, r = resid)
    df3 <-data.frame(X1 = X1, X2 = X2, r = resid_noX1)
    df4 <-data.frame(X1 = X1, X2 = X2, r = resid_noX2)
    
    ## Set lim to enforace the same color scale within p1-p10
    lim <- c(min(c(Y, predY))+.75, max(c(Y,predY))-.7)
    
    ## First set of plots (prediction surfaces)
    p1 <- ggplot(df, aes(X1, X2, fill = trueY)) + coord_fixed() + geom_raster() + scale_fill_viridis(limits = lim, option = "C") +geom_contour(aes(z = trueY)) + labs(fill = "Y", title = "Truth")  +theme(plot.margin=unit(c(0,0,0,0), "cm"))
    p2 <- ggplot(df, aes(X1, X2, fill = predY)) + coord_fixed() + geom_raster() + scale_fill_viridis(limits = lim, option = "C") +geom_contour(aes(z = predY)) + labs(fill = "Y", title = "Using X1, X2") +theme(plot.margin=unit(c(0,0,0,0), "cm"))
    p3 <- ggplot(df, aes(X1, X2, fill = predY_noX2)) + coord_fixed() + geom_raster() + scale_fill_viridis(limits = lim, option = "C") +geom_contour(aes(z = predY_noX2)) + labs(fill = "Y", title = "Using X1") +theme(plot.margin=unit(c(0,0,0,0), "cm"))
    p4 <- ggplot(df, aes(X1, X2, fill = predY_noX1)) + coord_fixed() + geom_raster() + scale_fill_viridis(limits = lim, option = "C") +geom_contour(aes(z = predY_noX1)) + labs(fill = "Y", title = "Using X2") +theme(plot.margin=unit(c(0,0,0,0), "cm"))
    
    ## Second set of plots (residuals)
    p5 <- ggplot(df2, aes(X1, r))  + geom_point() + geom_smooth(method = 'loess', se = FALSE) + ggtitle("Using X1, X2")
    p6 <- ggplot(df2, aes(X2, r))  + geom_point() + geom_smooth(method = 'loess', se = FALSE) + ggtitle("Using X1, X2")
    p7 <- ggplot(df4, aes(X1, r))  + geom_point() + geom_smooth(method = 'loess', se = FALSE) + ggtitle("Using X1")
    p8 <- ggplot(df4, aes(X2, r))  + geom_point() + geom_smooth(method = 'loess', se = FALSE) + ggtitle("Using X1")
    p9 <- ggplot(df3, aes(X1, r))  + geom_point() + geom_smooth(method = 'loess', se = FALSE) + ggtitle("Using X2")
    p10 <- ggplot(df3, aes(X2, r))  + geom_point() + geom_smooth(method = 'loess', se = FALSE) + ggtitle("Using X2")
    
    ## Use radio but input to determine which set of plots to display
    if (input$but == "Prediction Surface"){
      gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
    } else {
      gridExtra::grid.arrange(p5, p6, p7, p8, p9, p10, nrow = 3)
    }
  })
  
}


# Run the app ----
shinyApp(ui = ui, server = server)