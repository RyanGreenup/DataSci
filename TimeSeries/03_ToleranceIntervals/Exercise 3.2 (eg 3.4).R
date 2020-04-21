#install.packages(EnvStats)

# Set Working Directory and Load Packages ---------------------------------


  # Remove Latent Variables (except for the preamble trigger)
  rm(list = setdiff(ls(), "load.pac.switch"))

  # Test and install pacman package
  if (require('pacman')) {
    library('pacman')
  } else{
    install.packages('pacman')
    library('pacman')
  }

  # Use Pacman to install other useful packages
  pacman::p_load(tidyverse, rmarkdown, dplyr, plotly, EnvStats, qcc, mise)

  mise()


### Import the Data Set
# library(readr)
# NickelConc <- read_csv("NickelConc.csv")
NickelConc <- read_csv(file = "../../0DataSets/NickelConc.csv")

# Create a control chart of the Sample Mean

qcc(NickelConc$Baseline, type = "xbar", std.dev = sd(NickelConc$Baseline), newdata = NickelConc$Compliance, confidence.level = 0.95 )

# Create a Cummulative Summation

cusum(NickelConc$Baseline, std.dev = sd(NickelConc$Baseline), decision.interval = 4, se.shift = 1, newdata = NickelConc$Compliance)












#Testing CUSUM vs xbar

backset <- rnorm(20, mean = 5.2, sd = 0.1)
testset <- rnorm(10, mean = 5.1, sd = 0.1)

qcc(backset, type = "xbar", std.dev = 0.1, newdata = testset, confidence.level = 0.95  )
cusum(backset, std.dev= 0.1, decision.interval = 4, se.shift = 1, newdata= testset)

  #Despite what you read, these both Seem to 'raise the alarm' in equal proportions
