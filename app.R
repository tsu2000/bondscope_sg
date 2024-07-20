library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(forecast)
library(xts)
library(gt)

# Load in raw data
data <- read.csv("https://raw.githubusercontent.com/tsu2000/bondscope_sg/main/data.csv")

# Create date groupings for data visualisation
data$Date <- as.Date(with(data, paste(Year, Month, Day, sep = "-")), format = "%Y-%m-%d")
data$Year.Month <- format(data$Date, "%Y-%m")

# Pivot from wide to long
df <- data %>%
  pivot_longer(
    cols = -c(Year, Month, Day, Date, Year.Month), # Columns to keep (not to pivot)
    names_to = "Bond.Type", # New column name for the pivoted columns
    values_to = "Value"    # New column name for the values
  )

# Rename values in the 'Bond Type' column using case_when
df <- df %>%
  mutate(Bond.Type = case_when(
    Bond.Type == "Overnight.Repo" ~ "Overnight Repo Yield",
    Bond.Type == "T.Bill.Yield.3.Mth" ~ "3-Month T-Bill Yield",
    Bond.Type == "T.Bill.Yield.6.Mth" ~ "6-Month T-Bill Yield",
    Bond.Type == "T.Bill.Yield.1.Yr" ~ "1-Year T-Bill Yield",
    Bond.Type == "Bond.Yield.2.Yr" ~ "2-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.2.Yr" ~ "2-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.5.Yr" ~ "5-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.5.Yr" ~ "5-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.7.Yr" ~ "7-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.7.Yr" ~ "7-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.10.Yr" ~ "10-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.10.Yr" ~ "10-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.15.Yr" ~ "15-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.15.Yr" ~ "15-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.20.Yr" ~ "20-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.20.Yr" ~ "20-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.30.Yr" ~ "30-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.30.Yr" ~ "30-Year SGS Bond Price",
    Bond.Type == "Bond.Yield.50.Yr" ~ "50-Year SGS Bond Yield",
    Bond.Type == "Bond.Price.50.Yr" ~ "50-Year SGS Bond Price",
    TRUE ~ Bond.Type  # Keep the original value if no condition is met
  ))

bond_opts <- c("Overnight Repo", "3-Month T-Bill", "6-Month T-Bill", "1-Year T-Bill", "2-Year SGS Bond", "5-Year SGS Bond", "7-Year SGS Bond", "10-Year SGS Bond", "15-Year SGS Bond", "20-Year SGS Bond", "30-Year SGS Bond", "50-Year SGS Bond")
time_periods <- c("All", "10 yrs", "5 yrs", "2 yrs", "1 yr", "6 mths", "3 mths", "1 mth")

existing_yields <- bond_opts[-c(1:2, 7)]

# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$link(
      rel = "shortcut icon",
      type = "image/png",
      href = "https://raw.githubusercontent.com/tsu2000/bondscope_sg/main/www/bondscope.png"
    )
  ),  
  navbarPage(
    title = tags$div(
      style = "display: flex;",
      img(src = "bondscope.png", style = "height: 1em; margin-right: 10px;"),
      div("BondScope SG", style = "font-family: Gill Sans, sans-serif; font-size: 20px; font-weight: bold; color: #FFC300;")
    ),
    tabPanel("📈 Data Trends",
             sidebarLayout(
               sidebarPanel(
                 tags$h3("📈 Data Trends for Singapore SGS Bonds/T-Bills"),
                 checkboxGroupInput("bond_choices", "Select bond types to be shown on graph:", choices = bond_opts, selected = c("2-Year SGS Bond", "5-Year SGS Bond", "10-Year SGS Bond")),
                 selectInput("time_period", "Select time period to display data:", choices = time_periods, selected = "1 yr"),
                 width = 3
               ),
               mainPanel(
                 plotlyOutput("yield_plot"),
                 plotlyOutput("price_plot"),
                 width = 9
               )
             )
    ),
    tabPanel("🎯 Forecasting",
             sidebarLayout(
               sidebarPanel(
                 tags$h3("🎯 Singapore Bond Yield Forecasting"),
                 selectInput("predict_choice", "Select bond type to predict yield rates for:", choices = existing_yields, selected = "1-Year T-Bill"),
                 markdown("Predicts the future bond yields of different types of Singapore bonds for the next month using historical daily yield rates and time series analysis with the `auto.arima` function in R. For more details about this function, view this resource <a href='https://otexts.com/fpp2/arima-r.html' target='_blank'>here</a>."),
                 width = 3
               ),
               mainPanel(
                 tags$h3("Plot of yield trend using automatic ARIMA"),
                 plotlyOutput("predict_plot"),
                 tags$h3("Predicted values"),
                 gt_output("predict_table"),
                 tags$h3("ARIMA Model Summary"),
                 verbatimTextOutput("arima_summary"),
                 tags$h3("ARIMA Model Residual Diagnostics"),
                 plotOutput("residuals_plot"),
                 verbatimTextOutput("ljung_box_test"),
                 width = 9
               )
             )
    ),
    tabPanel("📋 Data",
             fluidRow(
               align = "left",
               column(2, tags$h3("📋 All Yield Data")),
               column(10, div(downloadButton('downloadData', 'Download as .csv'), style = "margin-top: 15px;"))
             ),
             markdown("---"),
             dataTableOutput("data_table")
    ),
    tabPanel("❓About",
             tags$h1("🏛️ BondScope SG 💰"),
             tags$h3("A web application to view past trends and predict future movements for Singapore SGS Bond/T-Bill yields and prices."),
             markdown("<u>Definition of main financial products:</u>"),
             markdown("- **T-Bill**: A short-term bond issued by the Singapore government which matures in 6 months or 1 year, with no fixed coupon interest payments and is issued and traded at a discount to the par value.\n
                       - **SGS Bond**: A long-term bond issued by the Singapore government which matures in 2, 5, 10, 15, 20, 30 or 50 years, with fixed coupon interest payments given every 6 months, starting from the month of issue.\n
                       - **SSB**: Also known as *Singapore Savings Bonds*, SSB is a special type of bond with a 10-year maturity issued by the Singapore government with the special property of interest rates that increase the longer the bond is held, and can be redeemed at any time before maturity. Yields for SSB are indirectly tied to SGS Bond rates. For more information, visit <a href='https://www.ilovessb.com/' target='_blank'>this website</a>."),
             markdown("**Important information about rates**"),
             markdown(paste0("- Current daily yield data is from **January 1998 to ", format(max(data$Date), "%B %Y"), "** (Updated every month) and is based on benchmark prices and yields provided by MAS <a href='https://eservices.mas.gov.sg/statistics/fdanet/BenchmarkPricesAndYields.aspx' target='_blank'>here</a>.")),
             markdown("- Data reflects bid rates quoted by SGS Primary Dealers to each other based on standard market lot transactions as specified in the Rules and Market Practices of the SGS Market. Prices quoted to non-Primary Dealers may be different, subject to factors such as transaction size and administrative costs."),
             markdown("
                      - Figures before 2000 are the modes of closing bid prices quoted by SGS primary dealers.\n
                      - Figures after 2000 are the average of closing bid rates quoted by SGS primary dealers.\n
                      - Overnight repo rates are closing offer rates quoted by SGS primary dealers.\n
                      - The 7-year benchmark was discontinued on 1 February 2011.\n
                      - The final 3-month T-bill was issued on 20th June 2013.\n
                      - Issuance of 6-month T-bills ceased on 27th December 2013 and resumed on 2nd July 2019.\n
                      - The SGS Overnight Repo rate was discontinued on 1 January 2014.\n
                      - Yield is quoted as % p.a.\n
                      - Bond price is quoted in S$ per S$100 of principal amount, excluding any applicable accrued interest (i.e. on clean basis).
                    "),
             br(),
             markdown("For more information on Singapore government financial products, you may visit the Monetary Authority of Singapore (MAS) website <a href='https://www.mas.gov.sg/bonds-and-bills/compare-products-for-individuals' target='_blank'>here</a>.")
    )
  )
)

# Define server functions
server <- function(input, output, session) {
  
  # Render logo
  output$logo <- renderUI({
    tags$img(src = "data/bondscope.png", 
             alt = "BondScope SG Logo",
             style = "display: inline-block; vertical-align: middle; height: 30px; margin-right: 10px;"
    )
  })
  
  ### DATA PREPROCESSING FOR REACTIVE FUNCTIONS ###
  
  # Data pre-processing for trend visualisation
  yields <- reactive({
    paste0(input$bond_choices, " Yield")
  })
  prices <- reactive({
    bond_choices_filtered <- input$bond_choices[grep("Bond", input$bond_choices)]
    if (length(bond_choices_filtered) == 0) {
      return(character(0))  # Return an empty character vector if no bonds are selected
    }
    paste0(bond_choices_filtered, " Price")
  })
  start_date <- reactive({
    switch(input$time_period,
      "All" = min(df$Date),
      "10 yrs" = max(df$Date) - years(10),
      "5 yrs" = max(df$Date) - years(5), 
      "2 yrs" = max(df$Date) - years(2), 
      "1 yr" = max(df$Date) - years(1), 
      "6 mths" = max(df$Date) %m-% months(6), 
      "3 mths" = max(df$Date) %m-% months(3), 
      "1 mth" = max(df$Date) %m-% months(1)
    )
  })
  
  # Data pre-processing for forecasting
  predicted_yield <- reactive({
    paste0(input$predict_choice, " Yield")
  })
  predict_cutoff_date <- reactive({
    switch(input$predict_choice,
       "6-Month T-Bill" = as.Date("2019-06-27"),
       "1-Year T-Bill" = as.Date("1998-01-02"),
       "2-Year SGS Bond" = as.Date("1998-01-02"), 
       "5-Year SGS Bond" = as.Date("1998-01-02"), 
       "10-Year SGS Bond" = as.Date("1998-06-29"), 
       "15-Year SGS Bond" = as.Date("2001-08-28"), 
       "20-Year SGS Bond" = as.Date("2007-02-26"), 
       "30-Year SGS Bond" = as.Date("2012-03-28"),
       "50-Year SGS Bond" = as.Date("2022-08-12")
    )
  })
  full_trend <- reactive({
    df %>% filter(Bond.Type == predicted_yield() & Date >= predict_cutoff_date())
  })
  tts <- reactive({
    xts(full_trend()$Value, full_trend()$Date)
  })
  arima_model <- reactive({
    auto.arima(tts(), seasonal = FALSE)
  })
  check_residuals <- reactive({
    checkresiduals(arima_model())
  })
  check_residuals_summary <- reactive({
    checkresiduals(arima_model(), plot = FALSE)
  })
  date_vector <- reactive({
    seq.Date(from = max(full_trend()$Date) %m+% days(1), to = max(full_trend()$Date) %m+% months(1), by = "day")
  })
  arima_forecast <- reactive({
    forecast(arima_model(), h = length(date_vector()))
  })
  forecast_df <- reactive({
    afdf <- data.frame(arima_forecast())
    rownames(afdf) <- date_vector()
    afdf_final <- cbind(as.Date(rownames(afdf)), data.frame(afdf, row.names = NULL))
    names(afdf_final) <- c("Date", "Yield Rate Forecast", "80th Percentile (Low)", "80th Percentile (High)", "95th Percentile (Low)", "95th Percentile (High)")
    afdf_final
  })
  original_df <- reactive({
    odf <- data.frame(tts())
    odf_final <- cbind(as.Date(rownames(odf)), data.frame(odf, row.names = NULL))
    names(odf_final) <- c("Date", "Historical Yield Rate")
    odf_final
  })

  ### DATA VISUALISATION PLOTS ###
  
  # Display yield plot
  output$yield_plot <- renderPlotly({
    req(length(input$bond_choices) > 0)
    df %>%
      filter(Bond.Type %in% yields() & Date >= start_date()) %>%
      ggplot(aes(x = Date, y = Value, colour = Bond.Type)) +
      geom_line() +
      labs(
        x = "Year",
        y = "Yield (in %)",
        title = "Yield Trend over Time for SGS/T-Bills"
      ) +
      theme_bw()      
  })
  
  # Display price plot
  output$price_plot <- renderPlotly({
    req(length(input$bond_choices) > 0 & length(prices()) > 0)
    df %>% 
      filter(Bond.Type %in% prices() & Date >= start_date()) %>% 
      ggplot(aes(x = Date, y = Value, colour = Bond.Type)) +
      geom_line() + 
      labs(
        x = "Year",
        y = "Price (in SGD)",
        title = "Price Trend over Time for SGS/T-Bills"
      ) +
      theme_bw()
  })

  # Display prediction plot
  output$predict_plot <- renderPlotly({
    ggplot() +
      geom_ribbon(data = forecast_df(), aes(x = Date, ymax = `80th Percentile (High)`, ymin = `80th Percentile (Low)`), fill = "azure4", alpha = 0.75) +
      geom_ribbon(data = forecast_df(), aes(x = Date, ymax = `95th Percentile (High)`, ymin = `95th Percentile (Low)`), fill = "gray", alpha = 0.5) +
      geom_line(data = forecast_df(), aes(x = Date, y = `Yield Rate Forecast`), color = "red") +
      geom_line(data = original_df(), aes(x = Date, y = `Historical Yield Rate`), color = "blue") +
      labs(
        x = "Time",
        y = "Yield (in %)",
        title = paste0("Historical and Predicted Yield Rates using ARIMA over time for ", input$predict_choice)
      ) +
      theme_gray()
  })
  
  output$predict_table <- render_gt({
    forecast_df() %>% 
      gt(rowname_col = "Date") %>% 
      tab_header(
        title = markdown(paste0("**Predicted Yield Forecast (in %) for ", input$predict_choice, "**")),
        subtitle = paste0("From ", min(date_vector()), " to ", max(date_vector()))
      ) %>% 
      tab_spanner(label = "Predicted values at selected percentiles", columns = c(3:6)) %>% 
      tab_stubhead("Future Date") %>% 
      cols_width(everything() ~ px(120)) %>% 
      cols_align(align = "right", columns = everything()) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = list(cells_stub(), cells_stubhead())
      )
  })
  
  output$arima_summary <- renderPrint({
    summary(arima_model())
  })
  
  output$residuals_plot <- renderPlot({
    check_residuals()
  })
  
  output$ljung_box_test <- renderPrint({
    check_residuals_summary()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('sgs_data_', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(data[, -c(24, 25)], con, row.names = FALSE, na = "")
    }
  )
  
  # Display data table
  output$data_table <- renderDataTable({
    data.frame(data[, -c(24, 25)])
  })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)