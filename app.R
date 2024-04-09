# Libraries
library(shiny)
library(quantmod)
library(TTR)
library(datasets)
library(plotly)
library(ggplot2)
library(shinythemes) 
library(dplyr)
library(lubridate)

# I) UI PART OF THE APPLICATION
ui <- navbarPage(
  # TITLE OF THE PROJECT
  title = "API Driven Periodic - Stock Market Prediction of US Stocks",
  # Black Theme 
  theme = shinytheme("flatly"), 
  # 1) HOME TAB CONTENT
  tabPanel(
    "Home",
    mainPanel(
      # Header and background text
      h1("API Driven Periodic stock market prediction of US stocks"),
      h2("Welcome Viewers!"),
      h4("1) While our Stock Market Prediction tool isn't equipped for daily market movements or investment advice, it offers insightful projections of stock trends over intervals, guiding your financial journey with clarity and foresight."),
      h4("2) The forecasting model utilizes a linear regression algorithm, drawing insights from Yahoo API data to anticipate discernible trends."),
      h4("3) The app is made simple by integrating pretrained model weights to the server.R file, hence, faster prediction of data retrieved from API, also data is trained on the last 30 days from most recent date of retrieval"),
      br(),
      h2("Instructions for the Prediction Application:"),
      h4("1) Head to the Prediction tab and witness the power of our API-driven prediction algorithm."),
      h4("2) Utilize the dropdown menu to seamlessly explore predictions for the five designated stocks."),
      h4("3) Upon selection, observe the top 5 rows and accompanying plot, offering a detailed comparison of actual performance versus predictions."),
      h4("4) The model undergoes routine training, drawing from updates provided by the Yahoo API. Rest assured, its relevance will endure for months and years to come."),
      br(),
      h2("Data and Visualization:"),
      h4("1) You can view the historical dataset, summary and structure for 5 different stocks in 'Data' tab"),
      h4("2) You can view the Histogram, Boxplot, Scatterplot, and Linegraph for 5 different stocks in 'Visualization' tab"),
    )
  ),
  # 2) DATA TAB CONTENT
  tabPanel(
    "Data",
    # Output retrieved from Server 
    uiOutput("header_text"),
    # Sidebar layout
    sidebarLayout(
      sidebarPanel(
        # Select between Dataset, Summary and Structure from the dropdown list
        selectInput("data_type", "Select Data Operation:",
                    choices = c("Dataset", "Summary", "Structure"),
                    selected = "Dataset"),
        # Select between AAPL, AMZN, NVDA, GOOGL, MSFT from the dropdown list
        selectInput("dashboard_symbol", "Select Stock Symbol:",
                    choices = c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT"),
                    selected = "AAPL"),
        # Output retrived from server for globe icon hyperlink
        uiOutput("dashboard_globe_link")
      ),
      mainPanel(
        # Output retrived from server for dataset output
        uiOutput("selected_data_output")
      )
    )
  ),
  # 3) VISUALIZATION TAB CONTENT
  tabPanel(
    "Visualization",
    # Output retrived from server for header output
    uiOutput("header_text_1"),
    sidebarLayout(
      sidebarPanel(
        # Select between Histogram, Boxplot_of_30_days, ScatterPlot_of_30_days, LineGraph_of_30_days from the dropdown list
        selectInput("graph_type", "Select Graph Type:",
                    choices = c("Histogram", "Boxplot_of_30_days", "ScatterPlot_of_30_days", "LineGraph_of_30_days"),
                    selected = "Histogram"),
        # Select between AAPL, AMZN, NVDA, GOOGL, MSFT from the dropdown list
        selectInput("stock_symbol", "Select Stock Symbol:",
                    choices = c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT"),
                    selected = "AAPL"),
        # Output retrived from server for globe icon hyperlink 
        uiOutput("dashboard_viz_globe_link")
      ),
      mainPanel(
        # Output retrived from server for plot
        plotOutput("visualization_plot")
        
      )
    )
  ),
  # 4) PREDICTION TAB CONTENT
  tabPanel(
    "Prediction",
    # Output retrived from server for header text
    uiOutput("header_text_2"),
    sidebarLayout(
      sidebarPanel(
        # Select between AAPL, AMZN, NVDA, GOOGL, MSFT from the dropdown list
        selectInput("symbol", "Select Stock Symbol:",
                    choices = c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT"),
                    selected = "AAPL"),
        # Output retrived from server for globe icon hyperlink 
        uiOutput("globe_link")
      ),
      
      mainPanel(
        # Plot Output retrived from server
        plotlyOutput("stock_plot"),
        # Text Output retrived from server 
        textOutput("predicted_value"),
        br(),
        # Dataset table retrived from server  
        dataTableOutput("stock_table")
      )
    )
  )
)

# II) SERVER PART OF THE APPLICATION

server <- function(input, output, session){
  # Define reactive values to store data for each stock
  stocks_data <- reactiveValues(
    AAPL = NULL,
    AMZN = NULL,
    NVDA = NULL,
    GOOGL = NULL,
    MSFT = NULL
  )
  
  # Create Function to update symbol data
  retrieveData <- function(symbol) {
    # Retrieve data from Yahoo API for 60 days
    getSymbols(symbol, src = "yahoo", from = Sys.Date()-60, to = Sys.Date())
    
    # Extract the open price, close price and timestamp
    open <- Op(get(symbol))
    timestamp <- index(get(symbol))
    close <- Cl(get(symbol))
    
    # Feature engineering
    MA_seven <- SMA(open, n = 7)
    MA_fourteen <- SMA(open, n = 14)
    MA_twenty <- SMA(open, n = 20)
    SD_seven <- runSD(open, n = 7)
    SD_fourteen <- runSD(open, n = 14)
    SD_twenty <- runSD(open, n = 20)
    
    
    # Create data frame with calculated features
    latest_opening <- data.frame(
      timestamp = timestamp,
      open = open,
      close = close,
      MA_seven = MA_seven,
      MA_fourteen = MA_fourteen,
      MA_twenty = MA_twenty,
      SD_seven = SD_seven,
      SD_fourteen = SD_fourteen,
      SD_twenty = SD_twenty
    )
    
    # Drop rows with NA values
    latest_opening <- na.omit(latest_opening)
    
    # Select the latest date for prediction
    latest_opening <- tail(latest_opening, 30)
    
    # Rename columns
    colnames(latest_opening) <- c("timestamp", "open", "Close", "MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")
    
    # Only take the predictor/input variables 
    latest_opening_F <- latest_opening[, c('MA_seven','MA_fourteen',
                                           'MA_twenty',
                                           'SD_seven',
                                           'SD_fourteen',
                                           'SD_twenty', 'Close')]
    
    # Load the saved model weights for linear regression
    re_LR_model <- readRDS(paste0(symbol, "_LR.rds"))
    
    # Predict and add closing prices for the last 30 days
    last_30_days <- tail(latest_opening_F, 30)
    retrained_model <- update(re_LR_model, data = last_30_days)
    # Retrieve data for last 5 days
    predicted_close <- predict(retrained_model, newdata = tail(latest_opening, 5))
    latest_opening$predicted_close <- NA
    latest_opening$predicted_close[(nrow(latest_opening) - 4):nrow(latest_opening)] <- predicted_close
    # Store the data for the selected stock
    stocks_data[[symbol]] <- latest_opening
  }
  
  # ---------------------------- (PREDICT TAB - START) ----------------------------
  
  # Map the selectInput for 'dashboard_symbol', 'stock_symbol', 'symbol' such that
  # Whenever change in one tab, other tabs will also get the same symbol replaced
  observe({
    updateSelectInput(session, "dashboard_symbol", selected = input$symbol)
    updateSelectInput(session, "stock_symbol", selected = input$symbol)
    updateSelectInput(session, "symbol", selected = input$symbol)
    retrieveData(input$symbol)
  })
  # Map the selectInput for 'dashboard_symbol', 'stock_symbol', 'symbol' such that
  # Whenever change in one tab, other tabs will also get the same symbol replaced
  observe({
    updateSelectInput(session, "dashboard_symbol", selected = input$dashboard_symbol)
    updateSelectInput(session, "stock_symbol", selected = input$dashboard_symbol)
    updateSelectInput(session, "symbol", selected = input$dashboard_symbol)
    retrieveData(input$dashboard_symbol)
  })
  # Map the selectInput for 'dashboard_symbol', 'stock_symbol', 'symbol' such that
  # Whenever change in one tab, other tabs will also get the same symbol replaced
  observe({
    updateSelectInput(session, "dashboard_symbol", selected = input$stock_symbol)
    updateSelectInput(session, "stock_symbol", selected = input$stock_symbol)
    updateSelectInput(session, "symbol", selected = input$stock_symbol)
    retrieveData(input$stock_symbol)
  })
  
  # Link the Globe icon to hyperlink for yahoo finance, so when you dropdown list
  # changes, it will reroute to the correct link
  output$globe_link <- renderUI({
    tags$a(href = paste0("https://finance.yahoo.com/quote/", input$symbol),
           icon("globe"), target="_blank")
  })
  # Link the Globe icon to hyperlink for yahoo finance, so when you dropdown list
  # changes, it will reroute to the correct link
  output$dashboard_globe_link <- renderUI({
    tags$a(href = paste0("https://finance.yahoo.com/quote/", input$symbol),
           icon("globe"), target="_blank")
  })
  # Link the Globe icon to hyperlink for yahoo finance, so when you dropdown list
  # changes, it will reroute to the correct link
  output$dashboard_viz_globe_link <- renderUI({
    tags$a(href = paste0("https://finance.yahoo.com/quote/", input$symbol),
           icon("globe"), target="_blank")
  })
  
  
  # Plotting the Prediction Plot using Plotly
  output$stock_plot <- renderPlotly({
    # Retrieve the selected stock data from Yahoo API
    stock_data <- stocks_data[[input$symbol]]
    
    # Filter the data to include only the week of prediction
    prediction_week <- stock_data[stock_data$timestamp >= stock_data$timestamp[nrow(stock_data)] - 6, ]
    
    # Store it in Dataframe
    plot_data <- data.frame(
      x = prediction_week$timestamp,
      y_actual = prediction_week$Close,
      y_predicted = prediction_week$predicted_close
    )
    # Plot the data for 5 days
    plot_ly(data = plot_data, x = ~x) %>%
      add_lines(y = ~y_actual, name = 'Actual', color = I("blue")) %>%
      add_lines(y = ~y_predicted, name = 'Predicted', color = I("red")) %>%
      layout(
        title = paste("Stock prediction over the last five days for", input$symbol),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Close Price"),
        legend = list(x = 0.8, y = 1)
      )
  })
  # ---------------------------- (PREDICT TAB - END - TO BE CONTINUED) ----------------------------
  
  
  # ---------------------------- (HOME TAB - START) ----------------------------
  output$stock_data_table <- renderDataTable({
    symbol <- input$dashboard_symbol
    data_file <- switch(symbol,
                        "AAPL" = "AAPL.csv",
                        "AMZN" = "AMZN.csv",
                        "NVDA" = "NVDA.csv",
                        "GOOGL" = "GOOGL.csv",
                        "MSFT" = "MSFT.csv")
    read.csv(data_file)
  })
  
  # Function to update data based on Data operation
  selected_data <- reactive({
    symbol <- input$dashboard_symbol
    data_file <- switch(symbol,
                        "AAPL" = "AAPL.csv",
                        "AMZN" = "AMZN.csv",
                        "NVDA" = "NVDA.csv",
                        "GOOGL" = "GOOGL.csv",
                        "MSFT" = "MSFT.csv")
    data <- read.csv(data_file)
    switch(input$data_type,
           "Summary" = summary(data), 
           "Structure" = capture.output(str(data)),  
           "Dataset" = data
    )
  })
  
  # Render the appropriate output based on the selected data type
  output$selected_data_output <- renderUI({
    if (input$data_type == "Dataset") {
      dataTableOutput("stock_data_table")
    } else if (input$data_type == "Summary" || input$data_type == "Structure") {
      renderPrint({
        selected_data()
      })
    }
  })
  # ----------------------- (HOME TAB - END) ----------------------------
  
  # ----------------------- (VISUALIZATION TAB - START) ----------------------------
  #Load data for selected stock symbol
  data <- reactive({
    stock_data_1 <- switch(input$dashboard_symbol,
                           "AAPL" = read.csv("AAPL.csv"),
                           "AMZN" = read.csv("AMZN.csv"),
                           "NVDA" = read.csv("NVDA.csv"),
                           "GOOGL" = read.csv("GOOGL.csv"),
                           "MSFT" = read.csv("MSFT.csv"))
    # Convert Date column to Date format
    #stock_data$Date <- as.Date(stock_data$Date, format = "%Y-%m-%d")
  })
  
  
  # Plotting function for selected graph type
  output$visualization_plot <- renderPlot({
    graph_type <- input$graph_type
    stock_data_2 <- stocks_data[[input$symbol]]
    stock_data_1 <- data()
    if (graph_type == "Histogram") {
      hist(stock_data_1$Volume, col = "red", main = "Histogram of Volume for 30 days",breaks = 400, xlab = "Volume")
      # plot boxplot
    } else if (graph_type == "Boxplot_of_30_days") {
      boxplot(stock_data_2$Close, col ='red',main = "Boxplot of Closing Value for 30 days", ylab = "Close Value")
      # Plot scatterplot
    } else if (graph_type == "ScatterPlot_of_30_days") {
      
      plot(stock_data_2$timestamp,stock_data_2$Close, type = "p", col = "red" ,main = "Scatter Plot of last 30 days")
      # plot lineplot
    } else {
      plot(stock_data_2$timestamp, stock_data_2$Close, type = "l",col = "red", main = "LineGraph")
    }
  })
  
  # ----------------------- (VISUALIZATION TAB - END) ----------------------------
  
  # ----------------------- (HEADER TEXT - START) ----------------------------
  # HEADER TEXT IN DATA
  output$header_text <- renderUI({
    h3(paste("Historical Data of stocks -", input$dashboard_symbol))
  })
  # HEADER TEXT IN VISUALIZATION
  output$header_text_1 <- renderUI({
    h3(paste("Visualization of Historical stock data -", input$dashboard_symbol))
  })
  # HEADER TEXT IN PREDICTION
  output$header_text_2 <- renderUI({
    h3(paste("API driven Stock Prediction of", input$dashboard_symbol))
  })
  # ----------------------- (HEADER TEXT - END) ----------------------------
  
  # ----------------------- (PREDICTED TEXT - CONTINUATION) ----------------------------
  # Render table for prediction tab
  output$stock_table <- renderDataTable({
    # Retrieve the selected stock data
    stock_data <- stocks_data[[input$symbol]]
    
    # Return the last 5 rows of the data frame with selected columns
    tail(stock_data[, c('timestamp', 'open', 'Close', 'predicted_close')], 5)
  })
  # ----------------------- (PREDICTED TEXT - END) ----------------------------
}

# Run the application
shinyApp(ui = ui, server = server)