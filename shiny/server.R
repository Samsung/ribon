library(lpSolveAPI)  # lpSolveAPI (http://lpsolve.sourceforge.net/5.5/R.htm)
library(ggplot2)  # ggplot
library(reshape)  # melt

source("global.R")

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
    rawPrice[(rawPrice$Platform == input$platform) & (rawPrice$Region == input$region), ]
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
    
    thisMonth <- insts[1, "Month"]
    instsTotal <- insts[1:min(valueX, nrow(insts)), ]
    if (valueX > nrow(insts)) {
      lastMonth <- insts[nrow(insts), 1]
      for (month.i in 1:(valueX - nrow(insts))) {
        newInsts <- data.frame(Month = newYearMonth(lastMonth, month.i), matrix(0, nrow = 1, ncol = ncol(insts) - 1))
        names(newInsts) <- names(insts)
        instsTotal <- rbind(instsTotal, newInsts)
      }
    }
    instsODM.base <- instsTotal
    instsNM1.base <- instsTotal
    instsPM1.base <- instsTotal
    instsPM3.base <- instsTotal
    instsAM1.base <- instsTotal
    instsAM3.base <- instsTotal
    instsODM.opt <- instsTotal
    instsNU1.opt <- instsTotal
    instsPU1.opt <- instsTotal
    instsPU3.opt <- instsTotal
    instsAU1.opt <- instsTotal
    instsAU3.opt <- instsTotal
    instsNM1.opt <- instsTotal
    instsPM1.opt <- instsTotal
    instsPM3.opt <- instsTotal
    instsAM1.opt <- instsTotal
    instsAM3.opt <- instsTotal
    cost.base <- instsTotal
    cost.opt <- instsTotal
    instsODM.base[, -1] <- 0
    instsNM1.base[, -1] <- 0
    instsPM1.base[, -1] <- 0
    instsPM3.base[, -1] <- 0
    instsAM1.base[, -1] <- 0
    instsAM3.base[, -1] <- 0
    instsODM.opt[, -1] <- 0
    instsNU1.opt[, -1] <- 0
    instsPU1.opt[, -1] <- 0
    instsPU3.opt[, -1] <- 0
    instsAU1.opt[, -1] <- 0
    instsAU3.opt[, -1] <- 0
    instsNM1.opt[, -1] <- 0
    instsPM1.opt[, -1] <- 0
    instsPM3.opt[, -1] <- 0
    instsAM1.opt[, -1] <- 0
    instsAM3.opt[, -1] <- 0
    cost.base[, -1] <- 0
    cost.opt[, -1] <- 0
    
    for (inst in names(instsTotal)[-1]) {
      selectedPrices <- sum(prices$Instance == inst)
      if (selectedPrices != 1) {
        return(NULL)
      }
    }
    
    getObjFunc <- function(inst, startIdx, endIdx) {
      c(rep(prices[prices$Instance == inst, "On.Demand_Month"], valueX),
        prices[prices$Instance == inst, "No.UP.1Y_Month"] * pmin(12, valueX - seq(1:valueX) + 1),
        prices[prices$Instance == inst, "Part.UP.1Y_UP"] + 
          prices[prices$Instance == inst, "Part.UP.1Y_Month"] * pmin(12, valueX - seq(1:valueX) + 1),
        prices[prices$Instance == inst, "Part.UP.3Y_UP"] + 
          prices[prices$Instance == inst, "Part.UP.3Y_Month"] * pmin(36, valueX - seq(1:valueX) + 1),
        rep(prices[prices$Instance == inst, "All.UP.1Y_UP"], valueX),
        rep(prices[prices$Instance == inst, "All.UP.3Y_UP"], valueX))
    }
    withProgress(message = "Optimizing", value = 0, {
      for (inst in names(instsTotal)[-1]) {
        lpResult <- make.lp(0, 6 * valueX)
        set.objfn(lpResult, getObjFunc(inst))
        for (st.i in 1:valueX) {
          idx1y = max(1, st.i - 11)
          idx3y = max(1, st.i - 35)
          xt <- c(numeric(st.i - 1), 1, numeric(valueX - st.i))  # on-demand
          xt <- c(xt, numeric(idx1y - 1), rep(1, st.i - idx1y + 1), numeric(valueX - st.i))  # 1year-no-upfront
          xt <- c(xt, numeric(idx1y - 1), rep(1, st.i - idx1y + 1), numeric(valueX - st.i))  # 1year-partial-upfront
          xt <- c(xt, numeric(idx3y - 1), rep(1, st.i - idx3y + 1), numeric(valueX - st.i))  # 3year-partial-upfront
          xt <- c(xt, numeric(idx1y - 1), rep(1, st.i - idx1y + 1), numeric(valueX - st.i))  # 1year-all-upfront
          xt <- c(xt, numeric(idx3y - 1), rep(1, st.i - idx3y + 1), numeric(valueX - st.i))  # 3year-all-upfront
          rhs <- instsTotal[st.i, inst]
          if (!(is.null(rawUps)) & (st.i <= 11)) {
            for (pre.i in 1:(12 - st.i)) {
              preMonth <- newYearMonth(thisMonth, -pre.i)
              tmpNU1 <- rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, inst]
              tmpPU1 <- rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, inst]
              tmpAU1 <- rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, inst]
              tmpPU3 <- rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst]
              tmpAU3 <- rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst]
              rhs <- rhs - 
                ifelse(length(tmpNU1) > 0, tmpNU1, 0) -
                ifelse(length(tmpPU1) > 0, tmpPU1, 0) -
                ifelse(length(tmpAU1) > 0, tmpAU1, 0) -
                ifelse(length(tmpPU3) > 0, tmpPU3, 0) -
                ifelse(length(tmpAU3) > 0, tmpAU3, 0)
            }  
          }
          if (!(is.null(rawUps)) & (st.i <= 35)) {
            for (pre.i in max(1, 13 - st.i):(36 - st.i)) {
              preMonth <- newYearMonth(thisMonth, -pre.i)
              tmpPU3 <- rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst]
              tmpAU3 <- rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst]
              rhs <- rhs - 
                ifelse(length(tmpPU3) > 0, tmpPU3, 0) -
                ifelse(length(tmpAU3) > 0, tmpAU3, 0)
            } 
          }
          add.constraint(lpResult, xt, ">=", rhs)
        }
        xtNU1 <- c(numeric(1 * valueX), pmin(12, valueX - seq(1:valueX) + 1), numeric(4 * valueX))
        xtPU1 <- c(numeric(2 * valueX), pmin(12, valueX - seq(1:valueX) + 1), numeric(3 * valueX))
        xtPU3 <- c(numeric(3 * valueX), pmin(36, valueX - seq(1:valueX) + 1), numeric(2 * valueX))
        xtAU1 <- c(numeric(4 * valueX), pmin(12, valueX - seq(1:valueX) + 1), numeric(1 * valueX))
        xtAU3 <- c(numeric(5 * valueX), pmin(36, valueX - seq(1:valueX) + 1))
        rhsNU1 <- max(0, floor(sum(instsTotal[inst]) * input$maxNU1 * 0.01))
        rhsPU1 <- max(0, floor(sum(instsTotal[inst]) * input$maxPU1 * 0.01))
        rhsPU3 <- max(0, floor(sum(instsTotal[inst]) * input$maxPU3 * 0.01))
        rhsAU1 <- max(0, floor(sum(instsTotal[inst]) * input$maxAU1 * 0.01))
        rhsAU3 <- max(0, floor(sum(instsTotal[inst]) * input$maxAU3 * 0.01))
        if (!is.null(rawUps)) {
          for (pre.i in 1:11) {
            preMonth <- newYearMonth(thisMonth, -pre.i)
            tmpNU1 <- rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, inst] * min(valueX, 12 - pre.i)
            tmpPU1 <- rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, inst] * min(valueX, 12 - pre.i)
            tmpAU1 <- rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, inst] * min(valueX, 12 - pre.i)
            tmpPU3 <- rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst] * min(valueX, 36 - pre.i)
            tmpAU3 <- rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst] * min(valueX, 36 - pre.i)
            rhsNU1 <- ifelse(length(tmpNU1) > 0, max(0, rhsNU1 - tmpNU1), rhsNU1)
            rhsPU1 <- ifelse(length(tmpPU1) > 0, max(0, rhsPU1 - tmpPU1), rhsPU1)
            rhsAU1 <- ifelse(length(tmpAU1) > 0, max(0, rhsAU1 - tmpAU1), rhsAU1)
            rhsPU3 <- ifelse(length(tmpPU3) > 0, max(0, rhsPU3 - tmpPU3), rhsPU3)
            rhsAU3 <- ifelse(length(tmpAU3) > 0, max(0, rhsAU3 - tmpAU3), rhsAU3)
          }
          for (pre.i in 12:35) {
            preMonth <- newYearMonth(thisMonth, -pre.i)
            tmpPU3 <- rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst] * min(valueX, 36 - pre.i)
            tmpAU3 <- rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst] * min(valueX, 36 - pre.i)
            rhsPU3 <- ifelse(length(tmpPU3) > 0, max(0, rhsPU3 - tmpPU3), rhsPU3)
            rhsAU3 <- ifelse(length(tmpAU3) > 0, max(0, rhsAU3 - tmpAU3), rhsAU3)
          }
        }
        add.constraint(lpResult, xtNU1, "<=", rhsNU1)
        add.constraint(lpResult, xtPU1, "<=", rhsPU1)
        add.constraint(lpResult, xtPU3, "<=", rhsPU3)
        add.constraint(lpResult, xtAU1, "<=", rhsAU1)
        add.constraint(lpResult, xtAU3, "<=", rhsAU3)
        set.bounds(lpResult, lower = numeric(6 * valueX))
        solve(lpResult)
        instsNU1.opt[, inst] <- floor(get.variables(lpResult)[(1 * valueX + 1):(2 * valueX)])
        instsPU1.opt[, inst] <- floor(get.variables(lpResult)[(2 * valueX + 1):(3 * valueX)])
        instsPU3.opt[, inst] <- floor(get.variables(lpResult)[(3 * valueX + 1):(4 * valueX)])
        instsAU1.opt[, inst] <- floor(get.variables(lpResult)[(4 * valueX + 1):(5 * valueX)])
        instsAU3.opt[, inst] <- floor(get.variables(lpResult)[(5 * valueX + 1):(6 * valueX)])
        rm(lpResult)
        incProgress(1 / (ncol(instsTotal) - 1), detail = inst)
      }
    })
    for (month.i in 1:valueX) {
      if (!(is.null(rawUps)) & (month.i <= 11)) {
        for (pre.i in 1:(12 - month.i)) {
          preMonth <- newYearMonth(thisMonth, -pre.i)
          instsNM1.base[month.i, -1] <- instsNM1.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, -1])[-1]
          instsPM1.base[month.i, -1] <- instsPM1.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, -1])[-1]
          instsAM1.base[month.i, -1] <- instsAM1.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, -1])[-1]
          instsPM3.base[month.i, -1] <- instsPM3.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, -1])[-1]
          instsAM3.base[month.i, -1] <- instsAM3.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, -1])[-1]
        }
      }
      if (!(is.null(rawUps)) & (month.i <= 35)) {
        for (pre.i in max(1, 13 - month.i):(36 - month.i)) {
          preMonth <- newYearMonth(thisMonth, -pre.i)
          instsPM3.base[month.i, -1] <- instsPM3.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, -1])[-1]
          instsAM3.base[month.i, -1] <- instsAM3.base[month.i, -1] + 
            colSums(rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, -1])[-1]
        } 
      }
      instsNM1.opt[month.i, -1] <- instsNM1.base[month.i, -1] + colSums(instsNU1.opt[max(1, month.i - 11):month.i, ])[-1]
      instsPM1.opt[month.i, -1] <- instsPM1.base[month.i, -1] + colSums(instsPU1.opt[max(1, month.i - 11):month.i, ])[-1]
      instsPM3.opt[month.i, -1] <- instsPM3.base[month.i, -1] + colSums(instsPU3.opt[max(1, month.i - 35):month.i, ])[-1]
      instsAM1.opt[month.i, -1] <- instsAM1.base[month.i, -1] + colSums(instsAU1.opt[max(1, month.i - 11):month.i, ])[-1]
      instsAM3.opt[month.i, -1] <- instsAM3.base[month.i, -1] + colSums(instsAU3.opt[max(1, month.i - 35):month.i, ])[-1]
    }
    instsODM.base[, -1] <- pmax(0, as.matrix(instsTotal[, -1] - 
                                               instsNM1.base[, -1] - 
                                               instsPM1.base[, -1] - 
                                               instsPM3.base[, -1] - 
                                               instsAM1.base[, -1] - 
                                               instsAM3.base[, -1]))
    instsODM.opt[, -1] <- pmax(0, as.matrix(instsTotal[, -1] - 
                                              instsNM1.opt[, -1] - 
                                              instsPM1.opt[, -1] - 
                                              instsPM3.opt[, -1] - 
                                              instsAM1.opt[, -1] - 
                                              instsAM3.opt[, -1]))
    for (inst in names(instsTotal)[-1]) {
      cost.base[, inst] <- 
        instsODM.base[, inst] * prices[prices$Instance == inst, "On.Demand_Month"] +
        instsNM1.base[, inst] * prices[prices$Instance == inst, "No.UP.1Y_Month"] +
        instsPM1.base[, inst] * prices[prices$Instance == inst, "Part.UP.1Y_Month"] +
        instsPM3.base[, inst] * prices[prices$Instance == inst, "Part.UP.3Y_Month"]
      cost.opt[, inst] <- 
        instsODM.opt[, inst] * prices[prices$Instance == inst, "On.Demand_Month"] +
        instsPU1.opt[, inst] * prices[prices$Instance == inst, "Part.UP.1Y_UP"] +
        instsPU3.opt[, inst] * prices[prices$Instance == inst, "Part.UP.3Y_UP"] +
        instsAU1.opt[, inst] * prices[prices$Instance == inst, "All.UP.1Y_UP"] +
        instsAU3.opt[, inst] * prices[prices$Instance == inst, "All.UP.3Y_UP"] +
        instsNM1.opt[, inst] * prices[prices$Instance == inst, "No.UP.1Y_Month"] +
        instsPM1.opt[, inst] * prices[prices$Instance == inst, "Part.UP.1Y_Month"] +
        instsPM3.opt[, inst] * prices[prices$Instance == inst, "Part.UP.3Y_Month"]
    }
    thisUps <- rbind(data.frame(Pricing = "NU1", instsNU1.opt[1, ]),
                     data.frame(Pricing = "PU1", instsPU1.opt[1, ]),
                     data.frame(Pricing = "PU3", instsPU3.opt[1, ]),
                     data.frame(Pricing = "AU1", instsAU1.opt[1, ]),
                     data.frame(Pricing = "AU3", instsAU3.opt[1, ]))
    if (!is.null(rawUps)) {
      updateUps <- rbind(rawUps, thisUps)
    }
    else {
      updateUps <- thisUps
    }
    
    optResult <- list()
    optResult[["updateUps"]] <- updateUps
    optResult[["instsTotal"]] <- instsTotal
    optResult[["instsODM.base"]] <- instsODM.base
    optResult[["instsNM1.base"]] <- instsNM1.base
    optResult[["instsPM1.base"]] <- instsPM1.base
    optResult[["instsPM3.base"]] <- instsPM3.base
    optResult[["instsAM1.base"]] <- instsAM1.base
    optResult[["instsAM3.base"]] <- instsAM3.base
    optResult[["instsODM.opt"]] <- instsODM.opt
    optResult[["instsNU1.opt"]] <- instsNU1.opt
    optResult[["instsPU1.opt"]] <- instsPU1.opt
    optResult[["instsPU3.opt"]] <- instsPU3.opt
    optResult[["instsAU1.opt"]] <- instsAU1.opt
    optResult[["instsAU3.opt"]] <- instsAU3.opt
    optResult[["instsNM1.opt"]] <- instsNM1.opt
    optResult[["instsPM1.opt"]] <- instsPM1.opt
    optResult[["instsPM3.opt"]] <- instsPM3.opt
    optResult[["instsAM1.opt"]] <- instsAM1.opt
    optResult[["instsAM3.opt"]] <- instsAM3.opt
    optResult[["cost.base"]] <- cost.base
    optResult[["cost.opt"]] <- cost.opt
    optResult
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
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    cost.base <- optResult[["cost.base"]]
    cost.opt <- optResult[["cost.opt"]]
    
    costBase.sum <- sum(cost.base[, -1])
    costOpt.sum <- sum(cost.opt[, -1])
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
    instsTotal <- optResult[["instsTotal"]]
    cost.base <- optResult[["cost.base"]]
    cost.opt <- optResult[["cost.opt"]]
    
    cost.plot <- data.frame(Month = as.character(instsTotal[, 1]), 
                            Base = rowSums(cost.base) - instsTotal[, 1],
                            Opt = rowSums(cost.opt) - instsTotal[, 1],
                            Base.cum = rowSums(cumsum(cost.base)) - cumsum(instsTotal[, 1]),
                            Opt.cum = rowSums(cumsum(cost.opt)) - cumsum(instsTotal[, 1]))
    names(cost.plot) <- c("Month", "Base", "Opt", "Base(Cum.)", "Opt(Cum.)")
    cost.melt <- melt(cost.plot, id.vars = "Month")
    names(cost.melt) <- c("Month", "Algorithm", "Cost")
    ggplot(cost.melt, aes(Month, as.numeric(as.character(Cost)))) + 
      geom_line(aes(group = Algorithm, colour = Algorithm), size = 1) +
      geom_point(aes(group = Algorithm, colour = Algorithm), size = 4) +
      myTheme(0) + 
      xlab("Month") +
      ylab("Cost ($)") + 
      ggtitle("Monthly Cost")
  })
  
  # Output: Cost Type
  output$costTypePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    instsTotal <- optResult[["instsTotal"]]
    cost.base <- optResult[["cost.base"]]
    cost.opt <- optResult[["cost.opt"]]
    
    cost.plot <- data.frame(Type = names(instsTotal)[-1],
                            Base = colSums(cost.base)[-1],
                            Opt = colSums(cost.opt)[-1])
    cost.melt <- melt(cost.plot, id.vars = "Type")
    names(cost.melt) <- c("Type", "Algorithm", "Cost")
    ggplot(cost.melt, aes(Type, as.numeric(as.character(Cost)), fill = Algorithm)) + 
      geom_bar(stat = "identity", position = "dodge") +
      myTheme(90) + 
      xlab("Sever Type") +
      ylab("Cost ($)") + 
      ggtitle("Total Cost for each Server Type")
  })
  
  # Output: Instance Text
  output$instText <- renderPrint({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    instsTotal <- optResult[["instsTotal"]]
    instsNM1.base <- optResult[["instsNM1.base"]]
    instsPM1.base <- optResult[["instsPM1.base"]]
    instsPM3.base <- optResult[["instsPM3.base"]]
    instsAM1.base <- optResult[["instsAM1.base"]]
    instsAM3.base <- optResult[["instsAM3.base"]]
    instsODM.opt <- optResult[["instsODM.opt"]]
    instsNM1.opt <- optResult[["instsNM1.opt"]]
    instsPM1.opt <- optResult[["instsPM1.opt"]]
    instsPM3.opt <- optResult[["instsPM3.opt"]]
    instsAM1.opt <- optResult[["instsAM1.opt"]]
    instsAM3.opt <- optResult[["instsAM3.opt"]]
    
    usageOD.mean <- sum(instsODM.opt[, -1]) / sum(instsTotal[, -1]) * 100
    usageNU1.mean <- sum(instsNM1.opt[, -1]) / sum(instsTotal[, -1]) * 100
    usagePU1.mean <- sum(instsPM1.opt[, -1]) / sum(instsTotal[, -1]) * 100
    usagePU3.mean <- sum(instsPM3.opt[, -1]) / sum(instsTotal[, -1]) * 100
    usageAU1.mean <- sum(instsAM1.opt[, -1]) / sum(instsTotal[, -1]) * 100
    usageAU3.mean <- sum(instsAM3.opt[, -1]) / sum(instsTotal[, -1]) * 100
    usageNU1.min <- sum(instsNM1.base[, -1]) / sum(instsTotal[, -1]) * 100
    usagePU1.min <- sum(instsPM1.base[, -1]) / sum(instsTotal[, -1]) * 100
    usagePU3.min <- sum(instsPM3.base[, -1]) / sum(instsTotal[, -1]) * 100
    usageAU1.min <- sum(instsAM1.base[, -1]) / sum(instsTotal[, -1]) * 100
    usageAU3.min <- sum(instsAM3.base[, -1]) / sum(instsTotal[, -1]) * 100
    cat("Usage Rate of On-Demand Instances:", usageOD.mean, "%", "\n")
    cat("Usage Rate of No-Upfront (1-Year) Instances:", usageNU1.mean, "%", "(Previous RIs: ",  usageNU1.min, "%)", "\n")
    cat("Usage Rate of Partial-Upfront (1-Year) Instances:", usagePU1.mean, "%", "(Previous RIs: ",  usagePU1.min, "%)", "\n")
    cat("Usage Rate of Partial-Upfront (3-Year) Instances:", usagePU3.mean, "%", "(Previous RIs: ",  usagePU3.min, "%)", "\n")
    cat("Usage Rate of All-Upfront (1-Year) Instances:", usageAU1.mean, "%", "(Previous RIs: ",  usageAU1.min, "%)", "\n")
    cat("Usage Rate of All-Upfront (3-Year) Instances:", usageAU3.mean, "%", "(Previous RIs: ",  usageAU3.min, "%)", "\n")
  })
  
  # Output: Instance Month
  output$instTimePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    instsTotal <- optResult[["instsTotal"]]
    instsODM.opt <- optResult[["instsODM.opt"]]
    instsNM1.opt <- optResult[["instsNM1.opt"]]
    instsPM1.opt <- optResult[["instsPM1.opt"]]
    instsPM3.opt <- optResult[["instsPM3.opt"]]
    instsAM1.opt <- optResult[["instsAM1.opt"]]
    instsAM3.opt <- optResult[["instsAM3.opt"]]
    
    instNum.plot <- data.frame(Month = as.character(instsTotal[, 1]), 
                               Total = rowSums(instsTotal) - instsTotal[, 1],
                               OD = rowSums(instsODM.opt) - instsTotal[, 1],
                               N1 = rowSums(instsNM1.opt) - instsTotal[, 1],
                               P1 = rowSums(instsPM1.opt) - instsTotal[, 1],
                               P3 = rowSums(instsPM3.opt) - instsTotal[, 1],
                               A1 = rowSums(instsAM1.opt) - instsTotal[, 1],
                               A3 = rowSums(instsAM3.opt) - instsTotal[, 1])
    names(instNum.plot) <- c("Month", "Total", "On-Demand", "No-UP1Y", "Part-UP1Y", "Part-UP3Y", "All-UP1Y", "All-UP3Y")
    instNum.melt <- melt(instNum.plot, id.vars = "Month")
    names(instNum.melt) <- c("Month", "Instance", "Number")
    ggplot(instNum.melt, aes(Month, as.numeric(as.character(Number)))) + 
      geom_line(aes(group = Instance, colour = Instance), size = 1) +
      geom_point(aes(group = Instance, colour = Instance), size = 4) +
      myTheme(0) +
      xlab("Month") + 
      ylab("Number of Instances") +
      ggtitle("Monthly Number of Instances")
  })
  
  # Output: Instance Type
  output$instTypePlot <- renderPlot({
    insts <- insts.rct()
    optResult <- optResult.rct()
    if (is.null(insts)) return(NULL)
    if (is.null(optResult)) return(NULL)
    instsTotal <- optResult[["instsTotal"]]
    instsODM.opt <- optResult[["instsODM.opt"]]
    instsNM1.opt <- optResult[["instsNM1.opt"]]
    instsPM1.opt <- optResult[["instsPM1.opt"]]
    instsPM3.opt <- optResult[["instsPM3.opt"]]
    instsAM1.opt <- optResult[["instsAM1.opt"]]
    instsAM3.opt <- optResult[["instsAM3.opt"]]
    
    instNum.plot <- data.frame(Type = names(instsTotal)[-1],
                               OD = colSums(instsODM.opt)[-1],
                               N1 = colSums(instsNM1.opt)[-1],
                               P1 = colSums(instsPM1.opt)[-1],
                               P3 = colSums(instsPM3.opt)[-1],
                               A1 = colSums(instsAM1.opt)[-1],
                               A3 = colSums(instsAM3.opt)[-1])
    names(instNum.plot) <- c("Type", "On-Demand", "No-UP1Y", "Part-UP1Y", "Part-UP3Y", "All-UP1Y", "All-UP3Y")
    instNum.melt <- melt(instNum.plot, id.vars = "Type")
    names(instNum.melt) <- c("Type", "Instance", "Number")
    ggplot(instNum.melt, aes(Type, as.numeric(as.character(Number)), fill = Instance)) + 
      geom_bar(stat = "identity") + 
      myTheme(90) +
      xlab("Server Type") + 
      ylab("Number of Instances") +
      ggtitle("Number of Instances for each Server Type")
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



