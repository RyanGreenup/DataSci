
### Import the Data Set
#library(readr)
#Aldicarb <- read_csv("C:/Users/qb3jl/Dropbox/(Units/Spring 2017/Environmental Informatics (200038)/Weekly Material/Week 2/Exercise/R/Datasets/Aldicarb.csv")

### View the Data Frame
Aldicarb

##Assign the Variables
x_w1 <- Aldicarb$well1
x_w2 <- Aldicarb$well2
x_w3 <- Aldicarb$well3




####Perform the test
  ###Given that the wells are measured identically and are operated in the same fashion
  ###pooled variance could be appropriate,
  ###But the exemplar did not use it so it won't be used here.



  
###Well 1
  t.test(x_w1, alternative ="greater", mu=7,  conf.level = 0.95) #99.6% confidence level of exceeding MCL

###Well 2
  t.test(x_w2, alternative ="greater", mu = 7, conf.level = 0.95) #99.97% confidence level of exceeding MCL
  
###Well 3
  t.test(x_w3, alternative ="less", mu = 7, conf.level = 0.95) #99.97% confidence level of exceeding MCL
  

  
  ###95%Confidence Interval of Well 2
  t_val <- qt((1-0.95)/2, 3, lower.tail=TRUE, log.p=FALSE)
  
  upper_w2 <-   mean(x_w2) + sd(x_w2)*t_val
  lower_w2 <-   mean(x_w2) - sd(x_w2)*t_val
  upper_w2
  lower_w2
    

  ###Confidence Interval Well 3
  t_well <- qt(0.025, 3, lower.tail = FALSE, log.p = FALSE)
  upper_w3 <-   mean(x_w3) + sd(x_w3)*t_val
  lower_w3 <-   mean(x_w3) - sd(x_w3)*t_val
  upper_w3
  lower_w3




