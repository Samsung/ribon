# Get basic theme for plot
#
# Args:
#   angle: Angle of axis text x and y
#
# Returns:
#   Theme of plot
myTheme <- function(angle) {
  theme.plot.title <- 
    theme(
      plot.title = 
        element_text(
          size = 16, face = "bold", vjust = 2, colour = "#3C3C3C"
        )
    )
  theme.axis.title.x <- 
    theme(
      axis.title.x = 
        element_text(
          size = 14, colour = "#535353", face = "bold", vjust = -0.5
        )
    )
  theme.axis.title.y <- 
    theme(
      axis.title.y = 
        element_text(
          size = 14, colour = "#535353", face = "bold", vjust = 1.5
        )
    )
  theme.axis.text.x <- 
    theme(
      axis.text.x = 
        element_text(
          size = 11, colour = "#535353", angle = angle
        )
    )
  theme.axis.text.y <- 
    theme(
      axis.text.y = 
        element_text(
          size = 11, colour = "#535353", angle = angle
        )
    )
  theme.legend.title <- 
    theme(
      legend.title = 
        element_text(
          size = 14, colour = "#535353", face = "bold"
        )
    )
  theme.legent.text <- 
    theme(
      legend.text = 
        element_text(size = 13, colour = "#535353"
        )
    )
  theme.plot.title + 
    theme.axis.title.x + theme.axis.title.y + 
    theme.axis.text.x + theme.axis.text.y +
    theme.legend.title + theme.legent.text
}

