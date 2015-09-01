source("R/output_text.R")

# Descript information about cost
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   NULL
descriptCost <- function(optResult) {
  costBase.sum <- outputCostSumBase(optResult)
  costOpt.sum <- outputCostSumOpt(optResult)
  costGain.mean <- (costBase.sum - costOpt.sum) / costBase.sum * 100
  
  cat("Total Cost of No RI Algorithm:", costBase.sum, "$", "\n")
  cat("Total Cost of Optimzed RI Algorithm:", costOpt.sum, "$", "\n")
  cat("Cost Gain:", costGain.mean, "%", "\n")
}

# Descript information about instance
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   NULL
descriptInstance <- function(optResult) {
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
}

