# Get sum of cost w/o optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Sum of cost w/o optimization
outputCostSumBase <- function(optResult) {
  cost.base <- optResult[["cost.base"]]
  sum(cost.base[, -1])
}

# Get sum of cost with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Sum of cost with optimization
outputCostSumOpt <- function(optResult) {
  cost.opt <- optResult[["cost.opt"]]
  sum(cost.opt[, -1])
}

# Get mean rate of On-demand instance usage with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of On-demand instance usage with optimization
outputRateMeanOptOD <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsODM.opt <- optResult[["instsODM.opt"]]
  sum(instsODM.opt[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of No-upfront 1-Year instance usage with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of No-upfront 1-Year instance usage with optimization
outputRateMeanOptNU1 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsNM1.opt <- optResult[["instsNM1.opt"]]
  sum(instsNM1.opt[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of Partial-Upfront 1-Year instance usage with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of Partial-Upfront 1-Year instance usage with optimization
outputRateMeanOptPU1 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsPM1.opt <- optResult[["instsPM1.opt"]]
  sum(instsPM1.opt[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of Partial-Upfront 3-Year instance usage with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of Partial-Upfront 13Year instance usage with optimization
outputRateMeanOptPU3 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsPM3.opt <- optResult[["instsPM3.opt"]]
  sum(instsPM3.opt[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of All-Upfront 1-Year instance usage with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of All-Upfront 1-Year instance usage with optimization
outputRateMeanOptAU1 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsAM1.opt <- optResult[["instsAM1.opt"]]
  sum(instsAM1.opt[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of All-Upfront 3-Year instance usage with optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of All-Upfront 3-Year instance usage with optimization
outputRateMeanOptAU3 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsAM3.opt <- optResult[["instsAM3.opt"]]
  sum(instsAM3.opt[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of No-Upfront 1-Year instance usage w/o optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of No-Upfront 1-Year instance usage w/o optimization
outputRateMeanBaseNU1 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsNM1.base <- optResult[["instsNM1.base"]]
  sum(instsNM1.base[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of Partial-Upfront 1-Year instance usage w/o optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of Partial-Upfront 1-Year instance usage w/o optimization
outputRateMeanBasePU1 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsPM1.base <- optResult[["instsPM1.base"]]
  sum(instsPM1.base[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of Partial-Upfront 3-Year instance usage w/o optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of Partial-Upfront 3-Year instance usage w/o optimization
outputRateMeanBasePU3 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsPM3.base <- optResult[["instsPM3.base"]]
  sum(instsPM3.base[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of All-Upfront 1-Year instance usage w/o optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of All-Upfront 1-Year instance usage w/o optimization
outputRateMeanBaseAU1 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsAM1.base <- optResult[["instsAM1.base"]]
  sum(instsAM1.base[, -1]) / sum(instsTotal[, -1]) * 100
}

# Get mean rate of All-Upfront 3-Year instance usage w/o optimization
#
# Args:
#   optResult: Results from optimization
#
# Returns:
#   Mean rate of All-Upfront 3-Year instance usage w/o optimization
outputRateMeanBaseAU3 <- function(optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsAM3.base <- optResult[["instsAM3.base"]]
  sum(instsAM3.base[, -1]) / sum(instsTotal[, -1]) * 100
}

