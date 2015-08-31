source("../R/date_calculation.R")
source("../R/plot_theme.R")
source("../R/data_access.R")
source("../R/optimization.R")
source("../R/output_plot.R")
source("../R/output_text.R")

shinyServer(function(input, output) {
  # Instance data read & refine
  insts.rct <- reactive({
    instsFile <- input$instsFile
    if (is.null(instsFile)) return(NULL)
    
    insts <- read.csv(instsFile$datapath, header = T, sep = ",")
    insts[is.na(insts)] <- 0  # NA to 0
    insts
  })
  
  # Upfront data read
  rawUps.rct <- reactive({
    upsFile <- input$upfrontsFile
    if (is.null(upsFile)) return(NULL)
    
    rawUps <- read.csv(upsFile$datapath, header = T, sep = ",")
    rawUps[is.na(rawUps)] <- 0  # NA to 0
    rawUps
  })
  
  # Simulation period setting
  valueX.rct <- reactive({
    input$simPeriod
  })
  
  # Prices setting
  prices.rct <- reactive({
    priceData <- rawPrice()
    priceData[
      (priceData$Platform == input$platform) & 
        (priceData$Region == input$region), 
      ]
  })
  
  # Optimization
  optResult.rct <- reactive({
    insts <- insts.rct()
    rawUps <- rawUps.rct()
    valueX <- valueX.rct()
    prices <- prices.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(valueX)) return(NULL)
    if (is.null(prices)) return(NULL)
    
    performOptimization(insts, rawUps, valueX, prices, 
                        input$maxNU1, input$maxPU1, input$maxPU3,
                        input$maxAU1, input$maxAU3)
  })
  
  # Output: Common notification
  output$commonNoti <- renderPrint({
    insts <- insts.rct()
    prices <- prices.rct()
    if (is.null(insts)) {
      cat("Please Input Expected Usage of Instances")
    } else {
      cat("Number of Server types:", ncol(insts) - 1, ", ")
      cat("Number of Months:", nrow(insts)) 
    }
    
    if (!is.null(prices)) {
      for (inst in names(insts)[-1]) {
        selectedPrices <- sum(prices$Instance == inst)
        if (selectedPrices == 0) {
          cat("\nNo price information:", inst)
        }
        else if (selectedPrices > 1) {
          cat("\nMore than 2 price infromation:", inst)
        }
      } 
    }
  })
  
  # Output: Usage of instances
  output$usageInsts <- renderDataTable({
    insts <- insts.rct()
    if (is.null(insts)) return(NULL)
    insts
  }, options = list(
    lengthMenu = list(c(10, -1), c("10", "All")),
    pageLength = 10,
    searching = F
  ))
  
  # Output: Previous upfronts
  output$preUpfronts <- renderDataTable({
    rawUps <- rawUps.rct()
    if (is.null(rawUps)) return(NULL)
    rawUps
  }, options = list(
    lengthMenu = list(c(10, -1), c("10", "All")),
    pageLength = 10,
    searching = F
  ))
  
  # Output: price data
  output$priceData <- renderDataTable({
    prices <- prices.rct()
    if (is.null(prices)) return(NULL)
    prices
  }, options = list(
    lengthMenu = list(c(10, 30, -1), c("10", "30", "All")),
    pageLength = 10
  ))
  
  # Output: Cost Text
  output$costText <- renderPrint({
    optResult <- optResult.rct()
    if (is.null(optResult)) return(NULL)
    
    costBase.sum <- outputCostSumBase(optResult)
    costOpt.sum <- outputCostSumOpt(optResult)
    costGain.mean <- (costBase.sum - costOpt.sum) / costBase.sum * 100
    
    cat("Total Cost of No RI Algorithm:", costBase.sum, "$", "\n")
    cat("Total Cost of Optimzed RI Algorithm:", costOpt.sum, "$", "\n")
    cat("Cost Gain:", costGain.mean, "%", "\n")
  })
  
  # Output: Cost Month
  output$costTimePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    
    plotCostTime(insts, optResult)
  })
  
  # Output: Cost Type
  output$costTypePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    
    plotCostType(insts, optResult)
  })
  
  # Output: Instance Text
  output$instText <- renderPrint({
    optResult <- optResult.rct()
    if (is.null(optResult)) return(NULL)
    
    usageOD.mean <- outputRateMeanOptOD(optResult)
    usageNU1.mean <- outputRateMeanOptNU1(optResult)
    usagePU1.mean <- outputRateMeanOptPU1(optResult)
    usagePU3.mean <- outputRateMeanOptPU3(optResult)
    usageAU1.mean <- outputRateMeanOptAU1(optResult)
    usageAU3.mean <- outputRateMeanOptAU3(optResult)
    usageNU1.min <- outputRateMeanBaseNU1(optResult)
    usagePU1.min <- outputRateMeanBasePU1(optResult)
    usagePU3.min <- outputRateMeanBasePU3(optResult)
    usageAU1.min <- outputRateMeanBaseAU1(optResult)
    usageAU3.min <- outputRateMeanBaseAU3(optResult)
    
    cat("Usage Rate of On-Demand Instances:", 
        usageOD.mean, "%", "\n")
    cat("Usage Rate of No-Upfront (1-Year) Instances:", 
        usageNU1.mean, "%", "(Previous RIs: ",  usageNU1.min, "%)", "\n")
    cat("Usage Rate of Partial-Upfront (1-Year) Instances:",
        usagePU1.mean, "%", "(Previous RIs: ",  usagePU1.min, "%)", "\n")
    cat("Usage Rate of Partial-Upfront (3-Year) Instances:", 
        usagePU3.mean, "%", "(Previous RIs: ",  usagePU3.min, "%)", "\n")
    cat("Usage Rate of All-Upfront (1-Year) Instances:", 
        usageAU1.mean, "%", "(Previous RIs: ",  usageAU1.min, "%)", "\n")
    cat("Usage Rate of All-Upfront (3-Year) Instances:", 
        usageAU3.mean, "%", "(Previous RIs: ",  usageAU3.min, "%)", "\n")
  })
  
  # Output: Instance Month
  output$instTimePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)

    plotInstTime(insts, optResult)
  })
  
  # Output: Instance Type
  output$instTypePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    
    plotInstType(insts, optResult)
  })
  
  # Output: Download upfronts
  output$dlUpfronts <- downloadHandler(
    filename = function() {
      paste("upfronts_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      optResult <- optResult.rct()
      if (is.null(optResult)) return(NULL)
      
      updateUps <- optResult[["updateUps"]]
      write.csv(updateUps, file)
    }
  )
})

