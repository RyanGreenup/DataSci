#install.packages(EnvStats)
library(EnvStats)



#Include the data frame
#library(readr)
#Temperature <- read_csv("Temperature.csv")

#Use the attach command to attach the data frame to R's variable search, i.e. enable tabbing.
attach(Temperature)

#Perform the Test
cor.test(Temperature$`Max Temperature`, Temperature$`Min Temperature`)


#It could be prudent to detach the Temperature library, or make that a habit...I guess
detach(Temperature)

#Roll the test into an if statement so we can get a printed response

sig_level <- 99 #Percentage probability of not making type 1 error.

if(
    cor.test(Temperature$`Max Temperature`, Temperature$`Min Temperature`)$p.value < 1-sig_level/100
                                            ){
                                                print("There is a signficant linear association between the variables")
                                                                    }
