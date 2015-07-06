library(ggplot2)  # ggplot
library(reshape)  # melt

# Get plot for cost per month
#
# Args:
#   inst: Instance usage
#   optResult: Results from optimization
#
# Returns:
#   Plot for cost per month
plotCostTime <- function(insts, optResult) {
  instsTotal <- optResult[["instsTotal"]]
  cost.base <- optResult[["cost.base"]]
  cost.opt <- optResult[["cost.opt"]]
  
  cost.plot <- data.frame(
    Month = as.character(instsTotal[, 1]), 
    Base = rowSums(cost.base) - instsTotal[, 1],
    Opt = rowSums(cost.opt) - instsTotal[, 1],
    Base.cum = rowSums(cumsum(cost.base)) - cumsum(instsTotal[, 1]),
    Opt.cum = rowSums(cumsum(cost.opt)) - cumsum(instsTotal[, 1])
  )
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
}

# Get plot for cost of each instance type
#
# Args:
#   inst: Instance usage
#   optResult: Results from optimization
#
# Returns:
#   Plot for cost of each instance type
plotCostType <- function(insts, optResult) {
  instsTotal <- optResult[["instsTotal"]]
  cost.base <- optResult[["cost.base"]]
  cost.opt <- optResult[["cost.opt"]]
  
  cost.plot <- data.frame(
    Type = names(instsTotal)[-1],
    Base = colSums(cost.base)[-1],
    Opt = colSums(cost.opt)[-1]
  )
  cost.melt <- melt(cost.plot, id.vars = "Type")
  names(cost.melt) <- c("Type", "Algorithm", "Cost")
  ggplot(cost.melt, 
         aes(Type, as.numeric(as.character(Cost)), fill = Algorithm)) + 
    geom_bar(stat = "identity", position = "dodge") +
    myTheme(90) + 
    xlab("Sever Type") +
    ylab("Cost ($)") + 
    ggtitle("Total Cost for each Server Type")
}

# Get plot for instance per month
#
# Args:
#   inst: Instance usage
#   optResult: Results from optimization
#
# Returns:
#   Plot for instance per month
plotInstTime <- function(insts, optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsODM.opt <- optResult[["instsODM.opt"]]
  instsNM1.opt <- optResult[["instsNM1.opt"]]
  instsPM1.opt <- optResult[["instsPM1.opt"]]
  instsPM3.opt <- optResult[["instsPM3.opt"]]
  instsAM1.opt <- optResult[["instsAM1.opt"]]
  instsAM3.opt <- optResult[["instsAM3.opt"]]
  
  instNum.plot <- data.frame(
    Month = as.character(instsTotal[, 1]), 
    Total = rowSums(instsTotal) - instsTotal[, 1],
    OD = rowSums(instsODM.opt) - instsTotal[, 1],
    N1 = rowSums(instsNM1.opt) - instsTotal[, 1],
    P1 = rowSums(instsPM1.opt) - instsTotal[, 1],
    P3 = rowSums(instsPM3.opt) - instsTotal[, 1],
    A1 = rowSums(instsAM1.opt) - instsTotal[, 1],
    A3 = rowSums(instsAM3.opt) - instsTotal[, 1]
  )
  names(instNum.plot) <- c("Month", 
                           "Total", 
                           "On-Demand", 
                           "No-UP1Y", 
                           "Part-UP1Y", 
                           "Part-UP3Y", 
                           "All-UP1Y", 
                           "All-UP3Y")
  instNum.melt <- melt(instNum.plot, id.vars = "Month")
  names(instNum.melt) <- c("Month", "Instance", "Number")
  ggplot(instNum.melt, aes(Month, as.numeric(as.character(Number)))) + 
    geom_line(aes(group = Instance, colour = Instance), size = 1) +
    geom_point(aes(group = Instance, colour = Instance), size = 4) +
    myTheme(0) +
    xlab("Month") + 
    ylab("Number of Instances") +
    ggtitle("Monthly Number of Instances")
}

# Get plot for instance of each instance type
#
# Args:
#   inst: Instance usage
#   optResult: Results from optimization
#
# Returns:
#   Plot for instance of each instance type
plotInstType <- function(insts, optResult) {
  instsTotal <- optResult[["instsTotal"]]
  instsODM.opt <- optResult[["instsODM.opt"]]
  instsNM1.opt <- optResult[["instsNM1.opt"]]
  instsPM1.opt <- optResult[["instsPM1.opt"]]
  instsPM3.opt <- optResult[["instsPM3.opt"]]
  instsAM1.opt <- optResult[["instsAM1.opt"]]
  instsAM3.opt <- optResult[["instsAM3.opt"]]
  
  instNum.plot <- data.frame(
    Type = names(instsTotal)[-1],
    OD = colSums(instsODM.opt)[-1],
    N1 = colSums(instsNM1.opt)[-1],
    P1 = colSums(instsPM1.opt)[-1],
    P3 = colSums(instsPM3.opt)[-1],
    A1 = colSums(instsAM1.opt)[-1],
    A3 = colSums(instsAM3.opt)[-1]
  )
  names(instNum.plot) <- c("Type", 
                           "On-Demand", 
                           "No-UP1Y", 
                           "Part-UP1Y", 
                           "Part-UP3Y", 
                           "All-UP1Y", 
                           "All-UP3Y")
  instNum.melt <- melt(instNum.plot, id.vars = "Type")
  names(instNum.melt) <- c("Type", "Instance", "Number")
  ggplot(instNum.melt, 
         aes(Type, as.numeric(as.character(Number)), fill = Instance)) + 
    geom_bar(stat = "identity") + 
    myTheme(90) +
    xlab("Server Type") + 
    ylab("Number of Instances") +
    ggtitle("Number of Instances for each Server Type")
}

