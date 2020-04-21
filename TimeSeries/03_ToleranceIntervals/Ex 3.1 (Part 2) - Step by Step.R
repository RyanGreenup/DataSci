#install.packages(EnvStats)
library(EnvStats)


#Create the Background Data frame

salmon_back <- rnorm(90, mean = 73*0.9, sd = 3)
basa_back <- rnorm(90, mean = 120*0.9, sd = 10)
cod_back <- rnorm(90, mean = 90*0.9, sd = 30)

fish_back <-  data.frame(salmon_back, basa_back, cod_back)

#Create the testing Data frame

salmon <- rnorm(30, mean = 73, sd = 3)
basa <- rnorm(30, mean = 120, sd = 10)
cod <- rnorm(30, mean = 90, sd = 30)

fish <-  data.frame(salmon, basa, cod)




#Create the Tolerance Intervals

LTL_salmon <- tolIntNorm(fish_back$salmon, coverage = 0.95, cov.type = "content", ti.type = "lower", conf.level = 0.95, method = "exact")$interval$limits[1]
LTL_cod <- tolIntNorm(fish_back$cod, coverage = 0.95, cov.type = "content", ti.type = "lower", conf.level = 0.95, method = "exact")$interval$limits[1]
LTL_basa <- tolIntNorm(fish_back$basa, coverage = 0.95, cov.type = "content", ti.type = "lower", conf.level = 0.95, method = "exact")$interval$limits[1]


#Compare the Tolerance Intervals, are they all less than the Tolerance limit?
all(fish$salmon < LTL_salmon)
all(fish$cod < LTL_cod)
all(fish$basa < LTL_basa)


#As they are all false, we can't conclude at our confidence interval that the fish are shorter (even though they are).









