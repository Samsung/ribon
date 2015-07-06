# Read price data
#
# Returns:
#   EC2 price data
rawPrice <- function() {
  avgDays <- 365 / 12  # Average days per month
  rawData <- read.csv("../data/ec2-price.csv", header = T, sep = ",")
  rawData <- cbind(rawData, 
                   "On.Demand_Month" = 
                     as.numeric(
                       substring(rawData[, "On.Demand_Hour"], 2, 6)
                     ) * 24 * avgDays)  # on-demand hour to month 
  rawData
}

