### Packages

# Average days per month
avgDays <- 365 / 12

# Price data load
rawPrice <- read.csv("ec2-price.csv", header = T, sep = ",")
rawPrice <- cbind(rawPrice, 
                  "On.Demand_Month" = 
                    as.numeric(substring(rawPrice[, "On.Demand_Hour"], 2, 6)) * 24 * avgDays) # on-demand hour to month

# Get previous or future yearmon
newYearMonth <- function(baseYearMonth, diffMonth) {
  baseYearMonth.year <- as.numeric(substr(baseYearMonth, 1, 4))
  baseYearMonth.month <- as.numeric(substr(baseYearMonth, 5, 6))
  diffMonth.year <- floor(diffMonth / 12)
  diffMonth.month <- diffMonth %% 12
  newYearMonth.year <- baseYearMonth.year + diffMonth.year
  newYearMonth.month <- baseYearMonth.month + diffMonth.month
  if (newYearMonth.month > 12) {
    newYearMonth.year <- newYearMonth.year + 1
    newYearMonth.month <- newYearMonth.month - 12
  }
  as.character(newYearMonth.year * 100 + newYearMonth.month)
}

# Theme for plot
myTheme <- function(angle) {
  theme(plot.title = element_text(size = 16, face = "bold", vjust = 2, colour = "#3C3C3C")) +
    theme(axis.title.x = element_text(size = 14, colour = "#535353", face = "bold", vjust = -0.5)) +
    theme(axis.title.y = element_text(size = 14, colour = "#535353", face = "bold", vjust = 1.5)) +
    theme(axis.text.x = element_text(size = 11, colour = "#535353", angle = angle)) +
    theme(axis.text.y = element_text(size = 11, colour = "#535353", angle = angle)) +
    theme(legend.title = element_text(size = 14, colour = "#535353", face = "bold")) +
    theme(legend.text = element_text(size = 13, colour = "#535353"))
}

