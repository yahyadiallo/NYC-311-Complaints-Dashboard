library(shiny)
library(RSocrata)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("NYC 311 Complaints Dashboard - 2023 & 2024"),
  
  sidebarLayout(
    sidebarPanel(
      p("This dashboard shows NYC 311 complaints data for 2023 and 2024."),
      p("Explore how residents submit complaints and the most frequent complaint types."),
      selectInput("selectedMonth", "Select Month", choices = c("All"), selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Communication Channels", plotOutput("pieChart")),
        tabPanel("Top Complaints", plotOutput("barChart")),
        tabPanel("Response Times", plotOutput("responseChart")),
        tabPanel("Agency Involvement", plotOutput("agencyChart")),
        tabPanel("About", 
                 h3("About This Dashboard"),
                 p("This interactive dashboard visualizes NYC 311 complaint data for 2023 and 2024."),
                 
                 h4("Statistical Analysis"),
                 p("- Response times were calculated using the difference between request creation and closure dates."),
                 p("- Complaint trends were analyzed by counting occurrences of each complaint type."),
                 p("- Agency involvement was determined by grouping complaints by agency and complaint type."),
                 p("- Communication channel data was visualized to show how residents submit requests."),
                 
                 h4("Authors"),
                 p("- Ryan Bell: Communication Channels"),
                 p("- Yahya Diallo: Response Times"),
                 p("- Kimberly Aurora Vargas: Complaint Trends"),
                 p("- Maximus Walton: Agency Involvement"),
                 
                 h4("Project Links"),
                 p("GitHub Repository: ", a("NYC 311 Data Challenge", href="https://github.com/UWB-Adv-Data-Vis-2025-Wi-B/data-challenge-nyc-complaints-option-1-rykm", target="_blank"))
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Load data using the original query
  data <- read.socrata("https://data.cityofnewyork.us/resource/erm2-nwe9.json?$limit=1000")
  
  # Create a new column 'month' extracted from created_date
  data$month <- format(as.Date(data$created_date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m")
  
  # Filter the data for only records from 2023 and 2024
  data <- data %>% filter(grepl("^(2023|2024)", month))
  
  # Update the month selection dropdown with unique months from the data
  monthChoices <- sort(unique(data$month))
  updateSelectInput(session, "selectedMonth", choices = c("All", monthChoices), selected = "All")
  
  # Reactive filtered data based on selected month
  filtered_data <- reactive({
    if (input$selectedMonth == "All") {
      data
    } else {
      data %>% filter(month == input$selectedMonth)
    }
  })
  
  ## --- PIE CHART: Communication Channels ---
  output$pieChart <- renderPlot({
    channel_counts <- filtered_data() %>%
      group_by(open_data_channel_type) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      mutate(percentage = round((count / sum(count)) * 100, 1),
             label = paste0(percentage, "%"))
    
    ggplot(channel_counts, aes(x = "", y = count, fill = open_data_channel_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Distribution of 311 Request Communication Channels (2023 & 2024)") +
      theme_void() +
      theme(legend.title = element_blank()) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5, color = "white")
  })
  
  ## --- BAR CHART: Most Common Complaints ---
  output$barChart <- renderPlot({
    top_complaints <- filtered_data() %>%
      group_by(complaint_type) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(top_complaints, aes(x = reorder(complaint_type, n), y = n)) +
      geom_col(fill = "#69b3a2") +
      coord_flip() +
      labs(
        title = "Most Common NYC Complaints (2023 & 2024)",
        x = "Complaint Type",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  ## --- BAR CHART: Response Times ---
  output$responseChart <- renderPlot({
    filtered_data() %>%
      mutate(
        created_date = as.POSIXct(created_date, format = "%Y-%m-%dT%H:%M:%S"),
        closed_date = as.POSIXct(closed_date, format = "%Y-%m-%dT%H:%M:%S"),
        response_time = as.numeric(difftime(closed_date, created_date, units = "days"))
      ) %>%
      filter(!is.na(response_time), response_time > 0, response_time < 21) %>%
      ggplot(aes(x = response_time)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "white", alpha = 0.7) +
      labs(title = "Response Time Distribution (2023 & 2024)", x = "Response Time (Days)", y = "Frequency") +
      theme_minimal()
  })
  
  ## --- BAR CHART: Agency Involvement ---
  output$agencyChart <- renderPlot({
    top_agencies <- filtered_data() %>%
      group_by(agency) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(top_agencies, aes(x = reorder(agency, n), y = n)) +
      geom_col(fill = "#ff7f50") +
      coord_flip() +
      labs(
        title = "Top 10 Agencies Handling 311 Complaints (2023 & 2024)",
        x = "Agency",
        y = "Number of Complaints"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
