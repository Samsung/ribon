library(shiny)

shinyUI(fluidPage(
  titlePanel("Ribon: RI Simulation Service"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        'instsFile', 
        'Expected Usage of Instances (Required)', 
        accept = c('text/csv')
      ),
      fileInput(
        'upfrontsFile', 
        'Previous Upfronts (Optional)', 
        accept = c('text/csv')
      ),
      tags$hr(),
      submitButton("Job Submit"), 
      tags$hr(),
      selectInput(
        "platform", 
        "Platform", 
        c("Linux", 
          "RHEL", 
          "SLES", 
          "Windows", 
          "Windows with SQL Standard", 
          "Windows with SQL Web")
      ),
      selectInput(
        "region", 
        "Region", 
        c("US East (N. Virginia)", 
          "US West (Oregon)", 
          "US West (Northern California)", 
          "EU (Ireland)", 
          "EU (Frankfurt)", 
          "Asia Pacific (Singapore)", 
          "Asia Pacific (Tokyo)", 
          "Asia Pacific (Sydney)", 
          "South America (Sao Paulo)", 
          "AWS GovCloud (US)")
      ),
      tags$hr(),
      numericInput(
        "simPeriod", 
        "Simulation Period", 
        12, 
        min = 1
      ),
      tags$hr(),
      sliderInput(
        "maxNU1", 
        "Max No-Upfront-1Year (%)", 
        min = 0, 
        max = 100, 
        value = 100
      ),
      sliderInput(
        "maxPU1", 
        "Max Partial-Upfront-1Year (%)", 
        min = 0, 
        max = 100, 
        value = 100
      ),
      sliderInput(
        "maxPU3", 
        "Max Partial-Upfront-3Year (%)", 
        min = 0, 
        max = 100, 
        value = 100
      ),
      sliderInput(
        "maxAU1", 
        "Max All-Upfront-1Year (%)", 
        min = 0, 
        max = 100, 
        value = 100
      ),
      sliderInput(
        "maxAU3", 
        "Max All-Upfront-3Year (%)", 
        min = 0, 
        max = 100, 
        value = 100
      ),
      tags$hr(),
      downloadButton(
        "dlUpfronts", 
        "Download (Upfronts)"
      )
    ),
    mainPanel(
      verbatimTextOutput("commonNoti"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Usage of Instances", 
          dataTableOutput("usageInsts")
        ),
        tabPanel(
          "Previous Upfronts",
          dataTableOutput("preUpfronts")
        ),
        tabPanel(
          "Price Infomation", 
          dataTableOutput("priceData")
        ),
        tabPanel(
          "Cost Analysis", 
          verbatimTextOutput("costText"), 
          tags$hr(),
          plotOutput("costTimePlot"), 
          tags$hr(),
          plotOutput("costTypePlot")
        ),
        tabPanel(
          "Instance Analysis", 
          verbatimTextOutput("instText"), 
          tags$hr(),
          plotOutput("instTimePlot"), 
          tags$hr(),
          plotOutput("instTypePlot")
        )
      )
    )
  )
))

