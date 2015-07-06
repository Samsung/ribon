# Get previous or future yearmon
#
# Args:
#   baseYearMonth: Year and month of the base time (e.g., 201507).
#   diffMonth: The months before or after from the base time (Integer).
#
# Returns:
#   The previous or future year and month (e.g., 201509)
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

