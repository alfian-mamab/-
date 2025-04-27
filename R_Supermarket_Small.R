library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(readxl)
library(lubridate) 

# Load Data
#setwd("D:/Data Analyst/Supermarket Project")
sales_data <- as.data.frame(read_excel("Supermarket_Data.xlsx")) %>%
  rename_with(~ gsub(" ", "_", .x))

# Ensure Date is in Date format
sales_data$Date <- as.Date(sales_data$Date, format="%m/%d/%Y")


# Define Tableau-like colors
tableau_blue <- "#4E79A7"
tableau_orange <- "#F28E2B"
tableau_green <- "#59A14F"
tableau_red <- "#E15759"
tableau_purple <- "#B07AA1"
tableau_brown <- "#9D7660"
tableau_pink <- "#D37295"
tableau_gray <- "#BAB0AC"
tableau_yellow <- "#FF9D4A"
tableau_light_blue <- "#86BCB6"

city_coords <- data.frame(
  City = c("Yangon", "Naypyitaw", "Mandalay"),
  Latitude = c(16.8661, 19.7633, 21.9750),
  Longitude = c(96.1951, 96.0785, 96.0836)
)
# Merge coordinates
sales_data <- sales_data %>% left_join(city_coords, by = "City")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Best Market Dashboard"),
  dashboardSidebar(
    sliderInput("date_range", "Period",
                min = min(sales_data$Date), max = max(sales_data$Date),
                value = c(min(sales_data$Date), max(sales_data$Date)),
                timeFormat="%Y-%m-%d"),
    selectInput("city", "City",
                choices = c("All", unique(sales_data$City)),
                selected = "All", multiple = FALSE),
    selectInput("product_type", "Product:",
                choices = c("All", unique(sales_data$Product_line)),
                selected = "All", multiple = FALSE)
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_transactions"),
      valueBoxOutput("total_quantity"),
      valueBoxOutput("total_gross_income")
    ),
    fluidRow(
      box(title = "Trend of Gross Income", plotOutput("gross_income_plot", height = "250px"), width = 12)
    ),
    fluidRow(
      box(title = "Customer Gender", plotOutput("gender_pie", height = "200px"), width = 3),
      box(title = "Customer Type", plotOutput("customer_type_pie", height = "200px"), width = 3),
      box(title = "Payment Method", plotOutput("payment_pie", height = "200px"), width = 3),
      box(title = "Ratings", width = 3, height = "270px",
          div(
            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
            tags$i(class = "fa fa-star", style = "font-size: 55px; color: orange; margin-bottom: 5px;"),
            tags$h1(style = "margin: 5px 0; font-weight: bold; color: #FFA500;", textOutput("rating_value"))
          )
      )
    ),
    fluidRow(
      box(title = "Product Sales by Type", plotOutput("product_sales_plot", height = "200px"), width = 4),
      box(title = "Cities", leafletOutput("sales_map", height = "200px"), width = 4),
      box(title="Transaction Time",plotOutput("heatmap", height = "200px"), width = 4)
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    data <- sales_data %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    if (!"All" %in% input$city) {
      data <- data %>% filter(City %in% input$city)
    }
    if (!"All" %in% input$product_type) {
      data <- data %>% filter(Product_line %in% input$product_type)
    }
    
    data
  })
  
  output$total_transactions <- renderValueBox({
    valueBox(format(nrow(filtered_data()), big.mark = ","), "Number of Transactions", icon = icon("shopping-cart"), color = "aqua")
  })
  
  output$total_quantity <- renderValueBox({
    valueBox(format(sum(filtered_data()$Quantity, na.rm = TRUE), big.mark = ","),
             "Quantity of Product Sold", icon = icon("boxes"),color = "aqua")
  })
  
  output$total_gross_income <- renderValueBox({
    valueBox(format(round(sum(filtered_data()$gross_income, na.rm = TRUE)), big.mark = ","), "Total Gross Income (USD)", icon = icon("dollar-sign"), color = "aqua")
  })
  
  output$gross_income_plot <- renderPlot({
    df <- filtered_data() %>%
      group_by(Date) %>%
      summarize(GrossIncome = sum(gross_income, na.rm = TRUE))
    
    ggplot(df, aes(x = Date, y = GrossIncome)) +
      geom_line(color = tableau_blue, size = 1) +
      labs(x = "Date", y = "Gross Income") +
      theme_minimal()
  })
  
  output$product_sales_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Product_line)) +
      geom_bar(fill = tableau_orange) +
      geom_text(stat = "count", aes(label = ..count..), hjust = 3, size = 3) +
      labs(x = "Product Type", y = "Count") +
      theme(axis.text.x = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_blank())+coord_flip()
  })
  
  output$gender_pie <- renderPlot({
    data_summary <- filtered_data() %>%
      count(Gender) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ggplot(data_summary, aes(x = "", y = percentage, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(fill = "Gender") +
      theme_void() +
      scale_fill_manual(values = c("Female" = tableau_blue, "Male" = tableau_orange)) +
      theme(legend.position = "bottom") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white")
  })
  
  output$customer_type_pie <- renderPlot({
    data_summary <- filtered_data() %>%
      count(Customer_type) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ggplot(data_summary, aes(x = "", y = percentage, fill = Customer_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(fill = "Type") +
      theme_void() +
      scale_fill_manual(values = c("Member" = tableau_orange, "Normal" = tableau_blue))+
      theme(legend.position = "bottom")+
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white")
  })
  
  output$payment_pie <- renderPlot({
    data_summary <- filtered_data() %>%
      count(Payment) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ggplot(data_summary, aes(x = "", y = percentage, fill = Payment)) +  
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(fill = "Method") +
      theme_void() + 
      scale_fill_manual(values = c("Cash" = tableau_blue, "Credit card" = tableau_orange, "Ewallet" = tableau_red)) +
      theme(legend.position = "bottom") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white")
  })
  
  output$rating_value <- renderText({
    round(mean(filtered_data()$Rating, na.rm = TRUE), 2)
  })
  
  output$sales_map <- renderLeaflet({
    req(nrow(filtered_data()) > 0) 
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        popup = ~paste0("<b>City:</b> ", City, "<br><b>Transactions:</b> ", n_distinct(Invoice_ID)),
        radius = ~sqrt(n_distinct(Invoice_ID)) * 0.5,
        fillColor = tableau_blue, 
        fillOpacity = 0.5, 
        weight = 0 
      ) %>%
      addLabelOnlyMarkers(
        lng = ~Longitude, lat = ~Latitude,
        label = ~City, 
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "bottom",
          textOnly = TRUE,
          style = list("font-weight" = 100, "font-size" = "9px", "color" = "black")
        )
      )
  })
  
  output$heatmap <- renderPlot({
    heatmap_data <- filtered_data() %>%
      group_by(Day, Hour) %>%
      summarize(Transactions = n_distinct(Invoice_ID))
    
    # Define day order and shorten names
    day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    heatmap_data$Day <- factor(heatmap_data$Day, levels = day_order, labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
    
    ggplot(heatmap_data, aes(x = Day, y = Hour, fill = Transactions)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Transactions), color = "black", size = 3) +
      scale_fill_gradient(low = "#FEE0D2", high = "#DE2D26") +
      labs(x = NULL, y = NULL) + theme_minimal() +
      theme(legend.position = "none")
  })
  
}

shinyApp(ui, server)
