# Read price data
#
# Args:
#   priceFile: Path of price data file
#
# Returns:
#   EC2 price data
rawPrice <- function(priceFile) {
  avgDays <- 365 / 12  # Average days per month
  rawData <- read.csv(priceFile, header = T, sep = ",")
  rawData <- cbind(rawData, 
                   "On.Demand_Month" = 
                     as.numeric(
                       substring(rawData[, "On.Demand_Hour"], 2, 6)
                     ) * 24 * avgDays)  # on-demand hour to month 
  rawData
}

