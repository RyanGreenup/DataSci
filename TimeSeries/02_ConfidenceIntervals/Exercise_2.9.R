
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
  
  ###Well 1 vs Well 3
    t.test(x_w1, x_w3, alternative ="two.sided", mu=0,  conf.level = 0.95)
  
  ###Well 1 vs Well 3 differing by more than 20 ppb
    
    t.test(x_w1, x_w3, alternative ="greater", mu=20,  conf.level = 0.95)
    
      ###What about 15?
  
        t.test(x_w1, x_w3, alternative ="greater", mu=15,  conf.level = 0.95)
  
       ###What about 10?
        
        t.test(x_w1, x_w3, alternative ="greater", mu=12.8793,  conf.level = 0.95)
  






