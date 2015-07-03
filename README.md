# ribon

Ribon is a simulation service for deriving the cost-optimized configuration of AWS EC2 instances using RI (Reserved Instance).

Currently, AWS provides five kinds of RI pricing policies, which is cost-efficient at most 77% compared with on-demand instance. If you input your expected usage of AWS EC2 instances, you can get the     cost-optimized RI configuration.

The project is written in R. Particularly, Shiny package is used for service dashboard and lpSolveAPI library is used for solving linear problem.

### Installation

To run the service, R should be installed (http://www.r-project.org/). Then, install four R packages:

```R
install.packages("lpSolveAPI")
install.packages("ggplot2")
install.packages("reshape")
install.packages("shiny")
library(shiny)
```

All your preparations are done. Run the service:

```R
runApp("shiny")
```

You can specify your IP address and port number (e.g., IP: 1.2.3.4, Port: 8788):

```R
runApp("shiny", port = 8788, host = "1.2.3.4")
```

You can see your service on http://1.2.3.4:8788.

### Usage
Choose the file "data/input-test.csv" as the expected usage of instances, then you can check the input and ouput information on the right panels.

### License
Published under GNU General Public License version 3.0 (GPLv3).
