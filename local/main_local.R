source("R/date_calculation.R")
source("R/plot_theme.R")
source("R/data_access.R")
source("R/optimization.R")
source("R/output_plot.R")
source("local/input_load.R")
source("local/output_descript.R")

# User input: file path of expected usage of instances
instsFile <- "data/input-test.csv"
instsFile <- "data/synthetic-control-downward.csv"
# User input: file path of previous upfront usage of instances
upsFile <- "data/input-upfront.csv"
# User input: whether applying previous upfronts
applyUps <- TRUE
# Platform of target ec2 instance
platform <- "Linux"
# Region of target ec2 instance
region <- "US East (N. Virginia)"
# User input: simulation period
valueX <- 24
# User input: maximum usage of percentage of no-upfront 1-year instances
maxNU1 <- 100
# User input: maximum usage of percentage of partial-upfront 1-year instances
maxPU1 <- 100
# User input: maximum usage of percentage of partial-upfront 3-year instances
maxPU3 <- 100
# User input: maximum usage of percentage of all-upfront 1-year instances
maxAU1 <- 100
# User input: maximum usage of percentage of all-upfront 3-year instances
maxAU3 <- 100

# Data load/refine/validate: expected usage of instances
insts <- loadInsts(instsFile)
# Data load/refine: previous upfront usage of instances
rawUps <- loadUpfronts(upsFile, applyUps)
# Data load/refine/validate: prices
prices <- loadPrices(insts, platform, region)
# Optimization
ptm <- proc.time()
optResult <- performOptimization(insts, rawUps, valueX, prices, 
                                 maxNU1, maxPU1, maxPU3, maxAU1, maxAU3, F)
proc.time() - ptm

# Output: expected usage of instances
insts
# Output: previous upfront usage of instances
rawUps
# Output: price
prices
# Output: description about cost
descriptCost(optResult)
# Output: plot cost per month
plotCostTime(insts, optResult)
# Output: plot cost per type
plotCostType(insts, optResult)
# Output: description about usage of instances
descriptInstance(optResult)
# Output: plot instances per month
plotInstTime(insts, optResult)
# Output: plot instances per type
plotInstType(insts, optResult)


