library(lpSolveAPI)  # lpSolveAPI (http://lpsolve.sourceforge.net/5.5/R.htm)

# Perform optimization and get ouputs
#
# Args:
#   insts: Instance usage
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: Maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: Maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: Maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: Maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: Maximum usage of percentage of all-upfront 3-year instances
#   printProg: Whether printing progress or not
#
# Returns
#   List that contains all the results of optimization
#     updateUps: 
#       Updated upfront instances
#     instsTotal: 
#       Total usage of instances
#     instsODM.base: 
#       Instance usage of On-demand per month (w/o Optimization)
#     instsNM1.base: 
#       Instance usage of No-upfront 1-Year per month (w/o Optimization)
#     instsPM1.base: 
#       Instance usage of Partial-upfront 1-Year per month (w/o Optimization)
#     instsPM3.base: 
#       Instance usage of Partial-upfront 3-Year per month (w/o Optimization)
#     instsAM1.base: 
#       Instance usage of All-upfront 1-Year per month (w/o Optimization)
#     instsAM3.base: 
#       Instance usage of All-upfront 3-Year per month (w/o Optimization)
#     instsODM.opt: 
#       Instance usage of On-demand per month (with Optimization)
#     instsNU1.opt: 
#       Upfront instances of No-upfront 1-Year (with Optimization)
#     instsPU1.opt: 
#       Upfront instances of Partial-upfront 1-Year (with Optimization)
#     instsPU3.opt: 
#       Upfront instances of Partial-upfront 3-Year (with Optimization)
#     instsAU1.opt: 
#       Upfront instances of All-upfront 1-Year (with Optimization)
#     instsAU3.opt: 
#       Upfront instances of All-upfront 3-Year (with Optimization)
#     instsNM1.opt: 
#       Instance usage of No-upfront 1-Year per month (with Optimization)
#     instsPM1.opt: 
#       Instance usage of Partial-upfront 1-Year per month (with Optimization)
#     instsPM3.opt: 
#       Instance usage of Partial-upfront 3-Year per month (with Optimization)
#     instsAM1.opt: 
#       Instance usage of All-upfront 1-Year per month (with Optimization)
#     instsAM3.opt: 
#       Instance usage of All-upfront 3-Year per month (with Optimization)
#     cost.base: 
#       Cost of each pricing policies per month (w/o Optimization)
#     cost.opt: 
#       Cost of each pricing policies per month (with Optimization)
performOptimization <- function(insts, rawUps, valueX, prices,
                                maxNU1, maxPU1, maxPU3, maxAU1, maxAU3,
                                printProg) {
  # Variables setting
  thisMonth <- insts[1, "Month"]
  optResult <- initOptResult(insts, valueX)
  
  # Check price information
  for (inst in names(optResult[["instsTotal"]])[-1]) {
    selectedPrices <- sum(prices$Instance == inst)
    if (selectedPrices != 1) {
      return(NULL)
    }
  }
  
  # Get upfronts
  if (printProg) {
    optResult <- getOptUpsWithProg(optResult, inst, rawUps, valueX, prices,
                                   maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                                   thisMonth)
  } else {
    optResult <- getOptUpsWithoutProg(optResult, inst, rawUps, valueX, prices,
                                      maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                                      thisMonth)
  }
  
  # Get instance usage per month
  optResult <- getInstanceUsage(optResult, rawUps, valueX, thisMonth)
  
  # Get cost
  optResult <- getCost(optResult, prices)
  
  # Get updated upfronts
  updateUpfronts(optResult, rawUps)
  
  optResult
}

# Set initial optimal results
#
# Args:
#   insts: Instance usage
#   valueX: Simulation period
#
# Returns
#   Initial optimal results
initOptResult <- function(insts, valueX) {
  optResult <- list()
  
  optResult[["instsTotal"]] <- insts[1:min(valueX, nrow(insts)), ]
  if (valueX > nrow(insts)) {
    lastMonth <- insts[nrow(insts), 1]
    for (month.i in 1:(valueX - nrow(insts))) {
      newInsts <- 
        data.frame(
          Month = newYearMonth(lastMonth, month.i), 
          matrix(0, nrow = 1, ncol = ncol(insts) - 1)
        )
      names(newInsts) <- names(insts)
      optResult[["instsTotal"]] <- rbind(optResult[["instsTotal"]], newInsts)
    }
  }
  
  optResult[["instsODM.base"]] <- optResult[["instsTotal"]]
  optResult[["instsNM1.base"]] <- optResult[["instsTotal"]]
  optResult[["instsPM1.base"]] <- optResult[["instsTotal"]]
  optResult[["instsPM3.base"]] <- optResult[["instsTotal"]]
  optResult[["instsAM1.base"]] <- optResult[["instsTotal"]]
  optResult[["instsAM3.base"]] <- optResult[["instsTotal"]]
  optResult[["instsODM.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsNU1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPU1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPU3.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAU1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAU3.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsNM1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPM1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsPM3.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAM1.opt"]] <- optResult[["instsTotal"]]
  optResult[["instsAM3.opt"]] <- optResult[["instsTotal"]]
  optResult[["cost.base"]] <- optResult[["instsTotal"]]
  optResult[["cost.opt"]] <- optResult[["instsTotal"]]
  
  optResult[["instsODM.base"]][, -1] <- 0
  optResult[["instsNM1.base"]][, -1] <- 0
  optResult[["instsPM1.base"]][, -1] <- 0
  optResult[["instsPM3.base"]][, -1] <- 0
  optResult[["instsAM1.base"]][, -1] <- 0
  optResult[["instsAM3.base"]][, -1] <- 0
  optResult[["instsODM.opt"]][, -1] <- 0
  optResult[["instsNU1.opt"]][, -1] <- 0
  optResult[["instsPU1.opt"]][, -1] <- 0
  optResult[["instsPU3.opt"]][, -1] <- 0
  optResult[["instsAU1.opt"]][, -1] <- 0
  optResult[["instsAU3.opt"]][, -1] <- 0
  optResult[["instsNM1.opt"]][, -1] <- 0
  optResult[["instsPM1.opt"]][, -1] <- 0
  optResult[["instsPM3.opt"]][, -1] <- 0
  optResult[["instsAM1.opt"]][, -1] <- 0
  optResult[["instsAM3.opt"]][, -1] <- 0
  optResult[["cost.base"]][, -1] <- 0
  optResult[["cost.opt"]][, -1] <- 0
  
  optResult
}

# Get objective function of LP problem
#
# Args:
#   inst: Instance usage
#   valueX: Simulation period
#   prices: EC2 price information
#
# Returns:
#   Objective function
getObjFunc <- function(inst, valueX, prices) {
  c(
    rep(
      prices[prices$Instance == inst, "On.Demand_Month"], 
      valueX
    ),
    prices[prices$Instance == inst, "No.UP.1Y_Month"] * 
      pmin(12, valueX - seq(1:valueX) + 1),
    prices[prices$Instance == inst, "Part.UP.1Y_UP"] + 
      prices[prices$Instance == inst, "Part.UP.1Y_Month"] * 
      pmin(12, valueX - seq(1:valueX) + 1),
    prices[prices$Instance == inst, "Part.UP.3Y_UP"] + 
      prices[prices$Instance == inst, "Part.UP.3Y_Month"] * 
      pmin(36, valueX - seq(1:valueX) + 1),
    rep(
      prices[prices$Instance == inst, "All.UP.1Y_UP"], 
      valueX
    ),
    rep(
      prices[prices$Instance == inst, "All.UP.3Y_UP"], 
      valueX)
  )
}

# Get optimized upfront with progress
#
# Args:
#   optResult: Optimal results
#   inst: target instance
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: maximum usage of percentage of all-upfront 3-year instances
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getOptUpsWithProg <- function(optResult, inst, rawUps, valueX, prices,
                              maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                              thisMonth) {
  withProgress(message = "Optimizing", value = 0, {
    for (inst in names(optResult[["instsTotal"]])[-1]) {
      optResult <- getOptUps(optResult, inst, rawUps, valueX, prices,
                             maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, thisMonth)
      incProgress(1 / (ncol(optResult[["instsTotal"]]) - 1), detail = inst)
    }
  })
  
  optResult
}

# Get optimized upfront without progress
#
# Args:
#   optResult: Optimal results
#   inst: target instance
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: maximum usage of percentage of all-upfront 3-year instances
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getOptUpsWithoutProg <- function(optResult, inst, rawUps, valueX, prices,
                                 maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                                 thisMonth) {
  for (inst in names(optResult[["instsTotal"]])[-1]) {
    optResult <- getOptUps(optResult, inst, rawUps, valueX, prices,
                           maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, thisMonth)
  }
  
  optResult
}

# Get optimized upfront instances
#
# Args:
#   optResult: Optimal results
#   inst: target instance
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   prices: EC2 price information
#   maxNU1: maximum usage of percentage of no-upfront 1-year instances
#   maxPU1: maximum usage of percentage of partial-upfront 1-year instances
#   maxPU3: maximum usage of percentage of partial-upfront 3-year instances
#   maxAU1: maximum usage of percentage of all-upfront 1-year instances
#   maxAU3: maximum usage of percentage of all-upfront 3-year instances
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getOptUps <- function(optResult, inst, rawUps, valueX, prices,
                      maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, 
                      thisMonth) {
  lpResult <- make.lp(0, 6 * valueX)
  set.objfn(lpResult, getObjFunc(inst, valueX, prices))
  for (st.i in 1:valueX) {
    idx1y = max(1, st.i - 11)
    idx3y = max(1, st.i - 35)
    xt <- c(numeric(st.i - 1), 
            1, 
            numeric(valueX - st.i))  # On-demand
    xt <- c(xt, 
            numeric(idx1y - 1), 
            rep(1, st.i - idx1y + 1), 
            numeric(valueX - st.i))  # No-upfront 1-Year
    xt <- c(xt, 
            numeric(idx1y - 1), 
            rep(1, st.i - idx1y + 1), 
            numeric(valueX - st.i))  # Partial-upfront 1-Year
    xt <- c(xt, 
            numeric(idx3y - 1), 
            rep(1, st.i - idx3y + 1), 
            numeric(valueX - st.i))  # Partial-upfront 3-Year
    xt <- c(xt, 
            numeric(idx1y - 1), 
            rep(1, st.i - idx1y + 1), 
            numeric(valueX - st.i))  # All-upfront 1-Year
    xt <- c(xt, 
            numeric(idx3y - 1), 
            rep(1, st.i - idx3y + 1), 
            numeric(valueX - st.i))  # All-upfront 3-Year
    rhs <- optResult[["instsTotal"]][st.i, inst]
    if (!(is.null(rawUps)) & (st.i <= 11)) {
      for (pre.i in 1:(12 - st.i)) {
        preMonth <- 
          newYearMonth(thisMonth, -pre.i)
        tmpNU1 <- 
          rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, inst]
        tmpPU1 <- 
          rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, inst]
        tmpAU1 <- 
          rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, inst]
        tmpPU3 <- 
          rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst]
        tmpAU3 <- 
          rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst]
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
        preMonth <- 
          newYearMonth(thisMonth, -pre.i)
        tmpPU3 <- 
          rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst]
        tmpAU3 <- 
          rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst]
        rhs <- rhs - 
          ifelse(length(tmpPU3) > 0, tmpPU3, 0) -
          ifelse(length(tmpAU3) > 0, tmpAU3, 0)
      } 
    }
    add.constraint(lpResult, xt, ">=", rhs)
  }
  xtNU1 <- c(numeric(1 * valueX), 
             pmin(12, valueX - seq(1:valueX) + 1), 
             numeric(4 * valueX))
  xtPU1 <- c(numeric(2 * valueX), 
             pmin(12, valueX - seq(1:valueX) + 1), 
             numeric(3 * valueX))
  xtPU3 <- c(numeric(3 * valueX), 
             pmin(36, valueX - seq(1:valueX) + 1), 
             numeric(2 * valueX))
  xtAU1 <- c(numeric(4 * valueX), 
             pmin(12, valueX - seq(1:valueX) + 1), 
             numeric(1 * valueX))
  xtAU3 <- c(numeric(5 * valueX), 
             pmin(36, valueX - seq(1:valueX) + 1))
  rhsNU1 <- max(0, floor(sum(optResult[["instsTotal"]][inst]) * maxNU1 * 0.01))
  rhsPU1 <- max(0, floor(sum(optResult[["instsTotal"]][inst]) * maxPU1 * 0.01))
  rhsPU3 <- max(0, floor(sum(optResult[["instsTotal"]][inst]) * maxPU3 * 0.01))
  rhsAU1 <- max(0, floor(sum(optResult[["instsTotal"]][inst]) * maxAU1 * 0.01))
  rhsAU3 <- max(0, floor(sum(optResult[["instsTotal"]][inst]) * maxAU3 * 0.01))
  if (!is.null(rawUps)) {
    for (pre.i in 1:11) {
      preMonth <- 
        newYearMonth(thisMonth, -pre.i)
      tmpNU1 <- 
        rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, inst] * 
        min(valueX, 12 - pre.i)
      tmpPU1 <- 
        rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, inst] * 
        min(valueX, 12 - pre.i)
      tmpAU1 <- 
        rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, inst] * 
        min(valueX, 12 - pre.i)
      tmpPU3 <- 
        rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst] * 
        min(valueX, 36 - pre.i)
      tmpAU3 <- 
        rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst] * 
        min(valueX, 36 - pre.i)
      rhsNU1 <- 
        ifelse(length(tmpNU1) > 0, max(0, rhsNU1 - tmpNU1), rhsNU1)
      rhsPU1 <- 
        ifelse(length(tmpPU1) > 0, max(0, rhsPU1 - tmpPU1), rhsPU1)
      rhsAU1 <- 
        ifelse(length(tmpAU1) > 0, max(0, rhsAU1 - tmpAU1), rhsAU1)
      rhsPU3 <- 
        ifelse(length(tmpPU3) > 0, max(0, rhsPU3 - tmpPU3), rhsPU3)
      rhsAU3 <- 
        ifelse(length(tmpAU3) > 0, max(0, rhsAU3 - tmpAU3), rhsAU3)
    }
    for (pre.i in 12:35) {
      preMonth <- 
        newYearMonth(thisMonth, -pre.i)
      tmpPU3 <- 
        rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, inst] * 
        min(valueX, 36 - pre.i)
      tmpAU3 <- 
        rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, inst] * 
        min(valueX, 36 - pre.i)
      rhsPU3 <- 
        ifelse(length(tmpPU3) > 0, max(0, rhsPU3 - tmpPU3), rhsPU3)
      rhsAU3 <- 
        ifelse(length(tmpAU3) > 0, max(0, rhsAU3 - tmpAU3), rhsAU3)
    }
  }
  add.constraint(lpResult, xtNU1, "<=", rhsNU1)
  add.constraint(lpResult, xtPU1, "<=", rhsPU1)
  add.constraint(lpResult, xtPU3, "<=", rhsPU3)
  add.constraint(lpResult, xtAU1, "<=", rhsAU1)
  add.constraint(lpResult, xtAU3, "<=", rhsAU3)
  set.bounds(lpResult, lower = numeric(6 * valueX))
  solve(lpResult)
  optResult[["instsNU1.opt"]][, inst] <- 
    floor(get.variables(lpResult)[(1 * valueX + 1):(2 * valueX)])
  optResult[["instsPU1.opt"]][, inst] <- 
    floor(get.variables(lpResult)[(2 * valueX + 1):(3 * valueX)])
  optResult[["instsPU3.opt"]][, inst] <- 
    floor(get.variables(lpResult)[(3 * valueX + 1):(4 * valueX)])
  optResult[["instsAU1.opt"]][, inst] <- 
    floor(get.variables(lpResult)[(4 * valueX + 1):(5 * valueX)])
  optResult[["instsAU3.opt"]][, inst] <- 
    floor(get.variables(lpResult)[(5 * valueX + 1):(6 * valueX)])
  rm(lpResult)
  
  optResult
}

# Get instance usage per month
#
# Args:
#   optResult: Optimal results
#   rawUps: Previous upfront instances
#   valueX: Simulation period
#   thisMonth: start month from simulation period
#
# Returns:
#   Optimal results
getInstanceUsage = function(optResult, rawUps, valueX, thisMonth) {
  for (month.i in 1:valueX) {
    if (!(is.null(rawUps)) & (month.i <= 11)) {
      for (pre.i in 1:(12 - month.i)) {
        preMonth <- newYearMonth(thisMonth, -pre.i)
        optResult[["instsNM1.base"]][month.i, -1] <- 
          optResult[["instsNM1.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "NU1" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsPM1.base"]][month.i, -1] <- 
          optResult[["instsPM1.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "PU1" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsAM1.base"]][month.i, -1] <- 
          optResult[["instsAM1.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "AU1" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsPM3.base"]][month.i, -1] <- 
          optResult[["instsPM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsAM3.base"]][month.i, -1] <- 
          optResult[["instsAM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, -1]
          )[-1]
      }
    }
    if (!(is.null(rawUps)) & (month.i <= 35)) {
      for (pre.i in max(1, 13 - month.i):(36 - month.i)) {
        preMonth <- newYearMonth(thisMonth, -pre.i)
        optResult[["instsPM3.base"]][month.i, -1] <- 
          optResult[["instsPM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "PU3" & rawUps$Month == preMonth, -1]
          )[-1]
        optResult[["instsAM3.base"]][month.i, -1] <- 
          optResult[["instsAM3.base"]][month.i, -1] + 
          colSums(
            rawUps[rawUps$Pricing == "AU3" & rawUps$Month == preMonth, -1]
          )[-1]
      } 
    }
    optResult[["instsNM1.opt"]][month.i, -1] <- 
      optResult[["instsNM1.base"]][month.i, -1] + 
      colSums(optResult[["instsNU1.opt"]][max(1, month.i - 11):month.i, ])[-1]
    optResult[["instsPM1.opt"]][month.i, -1] <- 
      optResult[["instsPM1.base"]][month.i, -1] + 
      colSums(optResult[["instsPU1.opt"]][max(1, month.i - 11):month.i, ])[-1]
    optResult[["instsPM3.opt"]][month.i, -1] <- 
      optResult[["instsPM3.base"]][month.i, -1] + 
      colSums(optResult[["instsPU3.opt"]][max(1, month.i - 35):month.i, ])[-1]
    optResult[["instsAM1.opt"]][month.i, -1] <- 
      optResult[["instsAM1.base"]][month.i, -1] + 
      colSums(optResult[["instsAU1.opt"]][max(1, month.i - 11):month.i, ])[-1]
    optResult[["instsAM3.opt"]][month.i, -1] <- 
      optResult[["instsAM3.base"]][month.i, -1] + 
      colSums(optResult[["instsAU3.opt"]][max(1, month.i - 35):month.i, ])[-1]
  }
  optResult[["instsODM.base"]][, -1] <- 
    pmax(0, as.matrix(optResult[["instsTotal"]][, -1] - 
                        optResult[["instsNM1.base"]][, -1] - 
                        optResult[["instsPM1.base"]][, -1] - 
                        optResult[["instsPM3.base"]][, -1] - 
                        optResult[["instsAM1.base"]][, -1] - 
                        optResult[["instsAM3.base"]][, -1]))
  optResult[["instsODM.opt"]][, -1] <- 
    pmax(0, as.matrix(optResult[["instsTotal"]][, -1] - 
                        optResult[["instsNM1.opt"]][, -1] - 
                        optResult[["instsPM1.opt"]][, -1] - 
                        optResult[["instsPM3.opt"]][, -1] - 
                        optResult[["instsAM1.opt"]][, -1] - 
                        optResult[["instsAM3.opt"]][, -1]))
  
  optResult
}

# Get cost
#
# Args:
#   optResult: Optimal results
#   prices: EC2 price information
#
# Returns:
#   Optimal results
getCost <- function(optResult, prices) {
  for (inst in names(optResult[["instsTotal"]])[-1]) {
    optResult[["cost.base"]][, inst] <- 
      optResult[["instsODM.base"]][, inst] * 
      prices[prices$Instance == inst, "On.Demand_Month"] +
      optResult[["instsNM1.base"]][, inst] * 
      prices[prices$Instance == inst, "No.UP.1Y_Month"] +
      optResult[["instsPM1.base"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.1Y_Month"] +
      optResult[["instsPM3.base"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.3Y_Month"]
    optResult[["cost.opt"]][, inst] <- 
      optResult[["instsODM.opt"]][, inst] * 
      prices[prices$Instance == inst, "On.Demand_Month"] +
      optResult[["instsPU1.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.1Y_UP"] +
      optResult[["instsPU3.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.3Y_UP"] +
      optResult[["instsAU1.opt"]][, inst] * 
      prices[prices$Instance == inst, "All.UP.1Y_UP"] +
      optResult[["instsAU3.opt"]][, inst] * 
      prices[prices$Instance == inst, "All.UP.3Y_UP"] +
      optResult[["instsNM1.opt"]][, inst] * 
      prices[prices$Instance == inst, "No.UP.1Y_Month"] +
      optResult[["instsPM1.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.1Y_Month"] +
      optResult[["instsPM3.opt"]][, inst] * 
      prices[prices$Instance == inst, "Part.UP.3Y_Month"]
  }
  
  optResult
}

# Update upfronts
#
# Args:
#   optResult: Optimal results
#   rawUps: Previous upfront instances
#
# Returns:
#   Optimal results
updateUpfronts <- function(optResult, rawUps) {
  thisUps <- 
    rbind(data.frame(Pricing = "NU1", optResult[["instsNU1.opt"]][1, ]),
          data.frame(Pricing = "PU1", optResult[["instsPU1.opt"]][1, ]),
          data.frame(Pricing = "PU3", optResult[["instsPU3.opt"]][1, ]),
          data.frame(Pricing = "AU1", optResult[["instsAU1.opt"]][1, ]),
          data.frame(Pricing = "AU3", optResult[["instsAU3.opt"]][1, ]))
  if (!is.null(rawUps)) {
    optResult[["updateUps"]] <- rbind(rawUps, thisUps)
  }
  else {
    optResult[["updateUps"]] <- thisUps
  }
  
  optResult
}


