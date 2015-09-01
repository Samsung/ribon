# Load/refine/validate expected usage of instances
#
# Args:
#   instsFile: file path of expected usage of instances
#
# Returns:
#   expected usage of instances
loadInsts <- function(instsFile) {
  insts <- read.csv(instsFile, header = T, sep = ",")
  
  insts[is.na(insts)] <- 0  # NA to 0
  
  if (is.null(insts)) {
    cat("Please Input Expected Usage of Instances")
  } else {
    cat("Number of Server types:", ncol(insts) - 1, ", ")
    cat("Number of Months:", nrow(insts)) 
  }
  
  insts
}

# Load/refine previous upfront usage of instances
#
# Args:
#   upsFile: file path of previous upfront usage of instances
#
# Returns:
#   previous upfront usage of instances
loadUpfronts <- function(upsFile) {
  rawUps <- read.csv(upsFile, header = T, sep = ",")
  
  rawUps[is.na(rawUps)] <- 0  # NA to 0
  
  rawUps
}

# Load/refine/validate prices
#
# Args:
#   insts: expected usage of instances
#   platform: platform of target ec2 instance
#   region: region of target ec2 instance
#
# Returns:
#   prices
loadPrices <- function(insts, platform, region) {
  priceFile <- "data/ec2-price.csv"
  priceData <- rawPrice(priceFile)
  
  prices <- priceData[
    (priceData$Platform == platform) & (priceData$Region == region), 
    ]
  
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
  
  prices
}