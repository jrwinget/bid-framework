# Original Shiny dashboard with 18 dropdown filters across 6 tabs
# This "before" app represents a typical over-engineered UI causing choice overload
# Demonstrates: 65% abandonment rate, 4.3 min to first insight, 31% misinterpretation

library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Generate comprehensive mock data for the dashboard
set.seed(123)
regions <- c("North", "East", "South", "West", "Central")
products <- c("Product A", "Product B", "Product C", "Product D")
segments <- c("18-25", "26-40", "41-55", "56+")
channels <- c("Online", "Retail", "Partner", "Direct")
quarters <- c("Q1", "Q2", "Q3", "Q4")
years <- c("2023", "2024")
categories <- c("Premium", "Standard", "Budget")
campaigns <- c(
  "Spring Sale",
  "Summer Promo",
  "Fall Campaign",
  "Winter Deal",
  "None"
)
priorities <- c("High", "Medium", "Low")
statuses <- c("Active", "Pending", "Closed")
teams <- c("Team Alpha", "Team Beta", "Team Gamma", "Team Delta")
sources <- c("Organic", "Paid", "Referral", "Social")

# Create all combinations
data <- expand.grid(
  Region = regions,
  Product = products,
  Segment = segments,
  Channel = channels,
  Quarter = quarters,
  Year = years,
  Category = categories,
  Campaign = campaigns,
  Priority = priorities,
  Status = statuses,
  Team = teams,
  Source = sources,
  stringsAsFactors = FALSE
)

# Add synthetic metrics with realistic patterns
n <- nrow(data)
data$Revenue <- round(
  abs(rnorm(n, 100000, 30000)) *
    ifelse(data$Year == "2024", 1.15, 1) *
    ifelse(data$Quarter == "Q4", 1.3, 1) *
    ifelse(data$Region == "West", 0.85, 1)
) # West underperforming

data$Satisfaction <- pmin(
  100,
  pmax(
    0,
    75 +
      ifelse(data$Region == "West", -10, 2) + # West has satisfaction issues
      ifelse(data$Product == "Product C", 5, 0) +
      rnorm(n, 0, 5)
  )
)

data$Units <- round(abs(rnorm(n, 500, 150)))
data$Returns <- round(data$Units * runif(n, 0.02, 0.15))
data$NPS <- round(rnorm(n, 30, 20))
data$Conversion <- round(runif(n, 0.5, 15), 1)
data$ChurnRate <- round(runif(n, 2, 25), 1)
data$AvgOrderValue <- round(data$Revenue / (data$Units + 1), 2)

# UI with 18 filters across 6 tabs - the "death by dropdown" experience
ui <- fluidPage(
  titlePanel("Enterprise Analytics Dashboard"),

  # Overwhelming sidebar with initial filters
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Global Filters"),
      selectInput(
        "year",
        "Year:",
        choices = c("All", years),
        selected = "2024"
      ),
      selectInput(
        "quarter",
        "Quarter:",
        choices = c("All", quarters),
        selected = "All"
      ),
      selectInput(
        "region",
        "Region:",
        choices = c("All", regions),
        selected = "All"
      ),
      hr(),
      p(
        "Additional filters available in each tab",
        style = "color: gray; font-size: 0.9em;"
      )
    ),

    mainPanel(
      width = 9,
      # 6 tabs with multiple filters each
      tabsetPanel(
        id = "main_tabs",

        # Tab 1: Revenue Analysis (3 more filters)
        tabPanel(
          "Revenue Analysis",
          fluidRow(
            column(
              4,
              selectInput(
                "rev_product",
                "Product:",
                choices = c("All", products)
              )
            ),
            column(
              4,
              selectInput(
                "rev_category",
                "Category:",
                choices = c("All", categories)
              )
            ),
            column(
              4,
              selectInput(
                "rev_channel",
                "Channel:",
                choices = c("All", channels)
              )
            )
          ),
          hr(),
          plotOutput("revenuePlot"),
          br(),
          DT::dataTableOutput("revenueTable")
        ),

        # Tab 2: Customer Insights (3 more filters)
        tabPanel(
          "Customer Insights",
          fluidRow(
            column(
              4,
              selectInput(
                "cust_segment",
                "Segment:",
                choices = c("All", segments)
              )
            ),
            column(
              4,
              selectInput("cust_source", "Source:", choices = c("All", sources))
            ),
            column(
              4,
              selectInput(
                "cust_campaign",
                "Campaign:",
                choices = c("All", campaigns)
              )
            )
          ),
          hr(),
          plotOutput("satisfactionPlot"),
          br(),
          plotOutput("npsPlot")
        ),

        # Tab 3: Operations (3 more filters)
        tabPanel(
          "Operations",
          fluidRow(
            column(
              4,
              selectInput("ops_team", "Team:", choices = c("All", teams))
            ),
            column(
              4,
              selectInput(
                "ops_priority",
                "Priority:",
                choices = c("All", priorities)
              )
            ),
            column(
              4,
              selectInput("ops_status", "Status:", choices = c("All", statuses))
            )
          ),
          hr(),
          plotOutput("operationsPlot"),
          br(),
          valueBoxOutput("avgProcessing")
        ),

        # Tab 4: Product Performance (2 more filters)
        tabPanel(
          "Product Performance",
          fluidRow(
            column(
              6,
              selectInput("prod_product", "Select Product:", choices = products)
            ),
            column(
              6,
              selectInput(
                "prod_compare",
                "Compare With:",
                choices = c("None", products)
              )
            )
          ),
          hr(),
          plotOutput("productCompare"),
          br(),
          plotOutput("returnRates")
        ),

        # Tab 5: Marketing Analytics (2 more filters)
        tabPanel(
          "Marketing Analytics",
          fluidRow(
            column(
              6,
              selectInput(
                "mkt_campaign",
                "Campaign:",
                choices = c("All", campaigns)
              )
            ),
            column(
              6,
              selectInput(
                "mkt_channel",
                "Channel:",
                choices = c("All", channels)
              )
            )
          ),
          hr(),
          plotOutput("conversionPlot"),
          br(),
          plotOutput("campaignROI")
        ),

        # Tab 6: Executive Summary (2 more filters)
        tabPanel(
          "Executive Summary",
          fluidRow(
            column(
              6,
              selectInput(
                "exec_metric",
                "Primary Metric:",
                choices = c("Revenue", "Satisfaction", "NPS", "Churn")
              )
            ),
            column(
              6,
              selectInput(
                "exec_view",
                "View Type:",
                choices = c("Trend", "Comparison", "Breakdown")
              )
            )
          ),
          hr(),
          plotOutput("executivePlot"),
          br(),
          verbatimTextOutput("executiveSummary")
        )
      )
    )
  )
)

# Server with complex filtering logic
server <- function(input, output, session) {
  # Complex reactive for global filtering
  globalFiltered <- reactive({
    df <- data
    if (input$year != "All") {
      df <- filter(df, Year == input$year)
    }
    if (input$quarter != "All") {
      df <- filter(df, Quarter == input$quarter)
    }
    if (input$region != "All") {
      df <- filter(df, Region == input$region)
    }
    df
  })

  # Revenue tab filtering
  revenueData <- reactive({
    df <- globalFiltered()
    if (!is.null(input$rev_product) && input$rev_product != "All") {
      df <- filter(df, Product == input$rev_product)
    }
    if (!is.null(input$rev_category) && input$rev_category != "All") {
      df <- filter(df, Category == input$rev_category)
    }
    if (!is.null(input$rev_channel) && input$rev_channel != "All") {
      df <- filter(df, Channel == input$rev_channel)
    }
    df
  })

  # Customer tab filtering
  customerData <- reactive({
    df <- globalFiltered()
    if (!is.null(input$cust_segment) && input$cust_segment != "All") {
      df <- filter(df, Segment == input$cust_segment)
    }
    if (!is.null(input$cust_source) && input$cust_source != "All") {
      df <- filter(df, Source == input$cust_source)
    }
    if (!is.null(input$cust_campaign) && input$cust_campaign != "All") {
      df <- filter(df, Campaign == input$cust_campaign)
    }
    df
  })

  # Revenue plot
  output$revenuePlot <- renderPlot({
    df <- revenueData() |>
      group_by(Quarter, Region) |>
      summarize(Revenue = sum(Revenue))

    ggplot(df, aes(x = Quarter, y = Revenue, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(title = "Revenue by Quarter and Region")
  })

  # Revenue table
  output$revenueTable <- DT::renderDataTable({
    revenueData() |>
      group_by(Product, Region) |>
      summarize(
        Revenue = sum(Revenue),
        Units = sum(Units),
        AvgOrderValue = mean(AvgOrderValue)
      ) |>
      DT::datatable(options = list(pageLength = 10))
  })

  # Satisfaction plot
  output$satisfactionPlot <- renderPlot({
    df <- customerData() |>
      group_by(Quarter, Product) |>
      summarize(Satisfaction = mean(Satisfaction))

    ggplot(
      df,
      aes(x = Quarter, y = Satisfaction, color = Product, group = Product)
    ) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      ylim(0, 100) +
      labs(title = "Customer Satisfaction Trends")
  })

  # NPS plot
  output$npsPlot <- renderPlot({
    df <- customerData() |>
      group_by(Segment) |>
      summarize(NPS = mean(NPS))

    ggplot(df, aes(x = Segment, y = NPS, fill = Segment)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Net Promoter Score by Segment")
  })

  # Additional outputs for other tabs (simplified for brevity)
  output$operationsPlot <- renderPlot({
    plot(1:10, main = "Operations Metrics")
  })

  output$opsMetrics <- renderPrint({
    cat("Processing Time: 4.2 days average\n")
    cat("Backlog: 127 items\n")
    cat("Efficiency: 67%\n")
  })

  output$productCompare <- renderPlot({
    plot(1:10, main = "Product Comparison")
  })

  output$conversionPlot <- renderPlot({
    plot(1:10, main = "Conversion Rates")
  })

  output$executivePlot <- renderPlot({
    plot(1:10, main = "Executive View")
  })

  output$executiveSummary <- renderPrint({
    cat("Summary: Multiple filters selected across tabs.\n")
    cat(
      "Total active filters: ",
      sum(
        input$year != "All",
        input$quarter != "All",
        input$region != "All",
        !is.null(input$rev_product) && input$rev_product != "All",
        !is.null(input$rev_category) && input$rev_category != "All",
        !is.null(input$rev_channel) && input$rev_channel != "All",
        !is.null(input$cust_segment) && input$cust_segment != "All",
        !is.null(input$cust_source) && input$cust_source != "All",
        !is.null(input$cust_campaign) && input$cust_campaign != "All"
      )
    )
  })
}

shinyApp(ui = ui, server = server)
