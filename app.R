library(shiny)
library(readxl)

# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage(navbarPage("Describing Numerical Data",
                           tabPanel("Data",
                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("header", "Column names in first row", TRUE),
                                        fileInput("file1", "Choose an Excel (.XLSX), Comma-Separated Values (.CSV), or Tab-Separated (.TXT) File",
                                                  multiple = FALSE,
                                                  accept = c(".csv",
                                                             ".txt",
                                                             ".xlsx")),
                                        # Horizontal line ----
                                        tags$hr(),
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),  
                                        p("First, if column names are not in the first row of your data, uncheck the box to the left. Otherwise, leave the box checked. Browse for an Excel (.XLSX), comma-separated values (.CSV), tab-separated (.TXT) file to import and use to describe numerical variables. If importing an Excel file, the data must be in the first sheet. Once a file is uploaded, a preview of the data (the first 6 observations) is shown."),  
                                        hr(),
                                        tableOutput("contents")
                                      ))),
                           tabPanel("Descriptive Statistics",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xvar", "Variable", NULL),
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),
                                        p("Choose the numerical variable that you would like to describe and visualize. Measures of central tendency, variation, and shape will display on the left side of the output. On the right side, a histogram and box plot will display."),  
                                        hr(),
                                        fluidRow(column(4,
                                                        h4("Descriptive Statistics"),
                                                        br(),
                                                        h4("Central Tendency"),
                                                        tableOutput("cent"),
                                                        
                                                        h4("Variation"),
                                                        tableOutput("var")),
                                                 
                                                 column(8,
                                                        h4("Visualizations"),
                                                        br(),
                                                        br(),
                                                        plotOutput("numplot"))),
                                        #fluidRow(column(4,
                                        #                h5("Variation"),
                                        #                tableOutput("var")),
                                        #         column(8,
                                        #                plotOutput("numplot3"))),
                                        fluidRow(column(4,
                                                        h4("Shape"),
                                                        tableOutput("shape")),
                                                 column(8,
                                                        plotOutput("numplot3")))
                                      )
                                    )),
                           tabPanel("Z-Scores - All Values",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xvar1", "Variable", NULL),
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),
                                        p("Choose the numerical variable that you would like to compute Z-Scores for. On the left, a table with original values and computed Z-Scores for all values in the variable chosen is displayed. If outliers are present, the table on the right will display the orginal values and Z-Scores for the outliers. If there are no outliers, the table will be empty."),  
                                        hr(),
                                        fluidRow(column(4,
                                                        h4("All Z-Scores"),
                                                        tableOutput("Z_All")),
                                                 column(8,
                                                        h4("Outlier Z-Scores"),
                                                        tableOutput("Outliers"))))))))
  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output){
    
    data <- reactive({
      req(input$file1)
      
      ext <- tools::file_ext(input$file1$name)
      switch(ext,
             csv = read.csv(input$file1$datapath, header = input$header),
             txt = read.delim(input$file1$datapath, header = input$header),
             xlsx = as.data.frame(read_xlsx(input$file1$datapath, sheet = 1, col_names = input$header, .name_repair = "universal")),
             validate("Invalid file; Please upload a .xlsx, .csv, or .txt file")
      )
    })
    
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "xvar", choices = colnames(Filter(is.numeric,data())), selected=colnames(Filter(is.numeric,data()))[1])
    })
    
    output$contents <- renderTable({
      head(data())}, spacing = "xs", align = "l") 
    
    modefun <- function(x){
      if(any(tabulate(match(x, unique(x))) > 1)){
        out <- unique(x)[which(tabulate(match(x, unique(x))) == max(tabulate(match(x, unique(x)))))]
        outp <- round(out,4)
        #outp <- paste(out, collapse =", ")
      } else {
        outp <- "None"}
      return(outp)
    }
    
    
    output$cent <- renderTable({
      xvarnum <- which(colnames(data()) %in% input$xvar)
      da <- data()[,xvarnum]
      mn <- mean(da, na.rm = TRUE)
      mdn <- median(da, na.rm = TRUE)
      md <- modefun(da)
      meas <- c("Mean", "Median", "Mode(s)")
      vals <- c(round(mn,4), round(mdn,4), toString(md))
      cbind(Measures = meas, Values = vals)
    }, digits = 4)
    
    output$var <- renderTable({
      xvarnumv <- which(colnames(data()) %in% input$xvar)
      dat <- data()[,xvarnumv]
      min <- min(dat)
      max <- max(dat)
      range <- max(dat) - min(dat)
      vari <- var(dat, na.rm = TRUE)
      std <- sd(dat, na.rm = TRUE)
      mn1 <- mean(dat, na.rm = TRUE)
      cova <- std/mn1 * 100
      Meas <- c("Range", "Variance", "Standard Deviation", " Coefficient of Variation")
      Vals <- c(round(range,4), round(vari,4), round(std,4), paste(round(cova,4), "%"))
      cbind(Measures = Meas, Values = Vals)
    }, digits = 4)
    
    
    output$shape <- renderTable({
      xvarnum2 <- which(colnames(data()) %in% input$xvar)
      dat2 <- data()[,xvarnum2]
      min1 <- min(dat2)
      q1 <- quantile(dat2, type = 6)[2] 
      q2 <- quantile(dat2, type = 6)[3] 
      q3 <- quantile(dat2, type = 6)[4]
      max1 <- max(dat2)
      iqr <- q3 - q1
      mn2 <- mean(dat2, na.rm = TRUE)
      sd2 <- sd(dat2, na.rm = TRUE)
      n <- length(dat2)
      lhs <- n/((n-1)*(n-2)) 
      skews <- lhs * sum(sapply(dat2, function(x) ((x-mn2)/sd2))^3)
      Meas1 <- c("Minimum", "Q1", "Q2", "Q3", "Maximum", "IQR", "Skewness")
      Vals1 <- c(round(min1, 4), round(q1, 4), round(q2, 4), round(q3, 4), round(max1, 4), round(iqr,4), round(skews, 4))
      cbind(Measures = Meas1, Values = Vals1)
    }, digits = 4)
    
    output$numplot <- renderPlot({
      xvarnum <- which(colnames(data()) %in% input$xvar)
      da <- data()[,xvarnum]
      mn <- mean(da, na.rm = TRUE)
      mdn <- median(da, na.rm = TRUE)
      plotout <- hist(data()[,xvarnum], main = paste0("Histogram: ", input$xvar), 
                      col ="lightgrey",
                      xlab = "", sub = "")
      plotout
      abline(v=mn, col="red", lwd=2, lty = 1)
      abline(v=mdn, col="purple", lwd=2, lty = 1)
      legend("topright", c("Mean", "Median"), col = c("red", "purple"), lty = c(1,1), lwd = c(2,2), cex = .7)
    })
    
    
    output$numplot3 <- renderPlot({
      xvarnum2 <- which(colnames(data()) %in% input$xvar)
      plotout2 <- boxplot(data()[,xvarnum2], main = paste0("Box Plot: ", input$xvar), 
                          col ="steelblue",
                          horizontal = TRUE,
                          pch = 19,
                          xlab = "")
      plotout2
    })
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "xvar1", choices = colnames(Filter(is.numeric,data())), selected=colnames(Filter(is.numeric,data()))[1])
    })
    
    output$Z_All <- renderTable({
      xvarnumz <- which(colnames(data()) %in% input$xvar1)
      daz <- data()[,xvarnumz]
      zscore <- scale(daz)
      ok <- data.frame(X_Values = daz, Z_Scores = round(zscore,4))
      ok
    }, spacing = "xs", align = "l", digits = 4) 
    
    output$Outliers <- renderTable({
      xvarnumz <- which(colnames(data()) %in% input$xvar1)
      daz <- data()[,xvarnumz]
      zscore <- scale(daz)
      ok <- data.frame(X_Values = daz, Z_Scores = round(zscore,4))
      outliersub <- ok[abs(ok$Z_Score) > 3,]
      outliersub
    }, spacing = "xs", align = "l", digits = 4) 
    
  }
    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)