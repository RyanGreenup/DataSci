# Preamble


if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(caret, scales, ggplot2, rmarkdown, shiny, ISLR, class, BiocManager, corrplot, plotly, tidyverse, latex2exp, stringr, reshape2, cowplot, ggpubr, rstudioapi, wesanderson, RColorBrewer, colorspace, gridExtra, grid, car, boot, colourpicker, tree, ggtree, mise, rpart, rpart.plot, knitr, MASS, magrittr, EnvStats, rstudioapi)
  #Mass isn't available for R 3.5...

set.seed(2655087) # Set the seed such that we might have reproducable results

# Wipe all the damn variables to prevent mistakes
mise()

kabstr <- function(df){
  
data.frame(variable = names(df),
           class = sapply(df, typeof),
           first_values = sapply(df, function(x) paste0(head(x),  collapse = ", ")),
           row.names = NULL) %>% 
  kable()
}  

select <- dplyr::select

# Question 1 Simple Linear Regression
  # 1. Which attributes have a linear association
layout(matrix(1))
cpu <- read.csv(file = "./Code/Datasets/CPU.csv", header = TRUE, sep = ",")
head(cpu)
str(cpu)
dim(cpu)
summary(cpu)


cpu_pretty <- cpu
names(cpu_pretty) <- c("Cycle\nPeriod", "Min\nMemory", "Max\nMemory", "Cache", "Min\nChannels", "Max\nChannels", "Performance")

corrplot(cor(cpu),method = 'ellipse', type = 'upper')

  ## Use Pairs
pairs(cpu)
    ### This is fucking useless, let's use a loop, perhaps recreate with ggplot2
layout(matrix(1:6, nrow = 2))
for (i in 1:6) {
plot(y=cpu$Performance, x = cpu[,i], xlab = names(cpu)[i], ylab = "Performance")
}


cpu.tidy <- pivot_longer(data = cpu, cols = 1:(ncol(cpu)-1))


# layout(matrix(1:6, nrow = 2))
# for (i in 1:6) {
# plot(y=cpu$Performance, x = cpu[,i], xlab = names(cpu)[i], ylab = "Performance")
# }

cpu_pretty$Performance <- log(cpu_pretty$Performance)
names(cpu_pretty)[7] <- "ln(Performance)"



subset <- names(cpu_pretty)[2:3]
cpu.tidymem <- pivot_longer(data = cpu_pretty[,c("ln(Performance)",subset)], cols = subset )
pmem <- ggplot(cpu.tidymem, aes(x = value, y = `ln(Performance)`, col = name)) +
  geom_point() + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  facet_grid(. ~ name ) +
  labs(col = "CPU \n Characteristic", x = NULL)


cpu.else <- dplyr::select(cpu_pretty, -subset)
cpu.tidyelse <- pivot_longer(data = cpu.else, cols = c(2,3,4))
pelse <- ggplot(cpu.tidyelse, aes(x = value, y = `ln(Performance)`, col = name)) +
  geom_point() + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  facet_grid(. ~ name, scales = "free_x") + 
  labs(col = "CPU \n Characteristic", x = NULL)

grid.arrange(pmem, pelse, nrow = 2)




#So we'll try maximum main memory, but, I won't like it, so instead I'll use frequency, then ill cross validate to compare many different types.
# There is compromises heteroskedasticity, let's consider a log transform
layout(matrix(1:6, nrow = 2))
for (i in 1:6) {
plot(y=sqrt(cpu$Performance), x = cpu[,i], xlab = names(cpu)[i], ylab = "Performance")
}

#Let's make all the models and return the models with the lowest RSE
models <- list()
for (i in 1:6) {
  models[[i]] <- (lm(cpu$Performance ~ cpu[,i]))

}

modelSum <- lapply(models, summary)
  
rsqvals <- rep(NA, 6)
for (i in 1:6) {
rsqvals[i] <- modelSum[i][[1]][["r.squared"]]
}
colnames(cpu)[rsqvals == min(rsqvals)]

rmsevals <- rep(NA, 6)
for (i in 1:6) {
rmsevals[i] <- mean((modelSum[[i]][["residuals"]])^2)
}
colnames(cpu)[rmsevals== min(rmsevals)] %>% round()

#Choose Maximum memory because it is significantly more linear,
# It looks heteroskedastic so use log transform

# So the best performing linear Model is "Cycle Time" and "Performance"




summary(lm(cpu$Performance ~ cpu[,1]))

# Make multiple plots, select the plot with the lowest R^2 and RSE value.

  # now of those choose the one with the lowest CV Error??/

# Let's consider cycle frequency
plot(y=cpu$Performance, x = (1/cpu$CycleTime))

#Fix it with a log transform
plot(y=Log(cpu$Performance), x = (1/cpu$CycleTime))

#None of them are very linear at all but frequency plays ball very well0i

# Question 2 Polynomial ---------------------------------------------------

# Choose Coefficients to determine how to model the data via polynomial regreession
  # In order to do this investigate the scatter plots



cpu <- read.csv(file = "./Code/Datasets/CPU.csv", header = TRUE, sep = ",")
head(cpu)
cor(cpu) %>% corrplot()

pairs(cpu, cex = 0.4)


#Look at a scatter plot of all variables
plot(Performance ~ (CycleTime), data = cpu)

plot(cpu$Performance, y = cpu[,2])

layout(matrix(1:6, nrow = 2))
for (i in 1:6) {
plot(y=cpu$Performance, x = cpu[,i], xlab = names(cpu)[2], ylab = "Performance")
}

# Yeah so this isn't very helpful whatsoever at all
  # The problem is the heteroskedasticity???

i <- 3
plot(y=cpu$Performance, x = log(cpu[,i]), xlab = names(cpu)[2], ylab = "Performance")


ggplot(data = cpu_pretty, aes(x = Frequency, y = Performance)) +
  geom_point() +
  theme_bw()
head(cpu_pretty)


validError <- function(model, dataframe, outputVar){
train <- sample(nrow(dataframe) * 0.45)
val.Error <- sqrt(mean(dataframe[-train, as.character(outputVar)] - predict(object = model, newdata = dataframe[-train,] ))) ; val.Error
}


dplyr::select(cpu, Performance) # Select is for Columns
dplyr::filter() # Filter is for Rows

surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)


cpu[-train, Performance]
names(cpu)
validError(model = cpu_mod.slm, dataframe = cpu, outputVar = Performance)

# Training Split
train <- sample(nrow(cpu) * 0.45)
val <- (1:nrow(cpu))[-train] 
if (!(c(train, val) %>% length() == nrow(cpu))) {
 print("The Split is Wrong") 
}


cpu_mod.slm <- lm(`ln(Performance)` ~ Frequency, data = cpu_pretty, subset = train)

cpu_pretty[-train,] %>% dplyr::select("ln(Performance)") - select(cpu_pretty[-train,])

(cpu_pretty %>% 
  dplyr::slice(-train) %>%
  dplyr::select("ln(Performance)") -
  predict(cpu_mod.slm, newdata =   cpu_pretty %>% 
  dplyr::slice(-train)  ))**2 %>% sum() / length(val) %>% sqrt()

predict(cpu_mod.slm, cpu_pretty[val,])
thisone <-filter(select(cpu_pretty, "ln(performance)"), val) - predict(cpu_mod.slm,cpu_pretty[val,])




((slice(select(cpu_pretty, "ln(Performance)"), val) -  predict(cpu_mod.slm,cpu_pretty[val,]))^2) %>%
  as_vector() %>%
  mean() %>% 
  sqrt()

validError <- function(dataframe, outputVar, model, ratio = 0.6, trainIndex){
((select(dataframe, as.character(outputVar))[-trainIndex,] -  predict(model,dataframe[-trainIndex,]))^2) %>%
  as_vector() %>%
  mean() %>% 
  sqrt()
  
}

validError(dataframe = cpu_pretty, model = cpu_mod.slm, outputVar = "ln(Performance)", trainIndex = train)

names(cpu_pretty)


# Return Model
cpu_mod.slm <- lm(Frequency ~ `ln(Performance)`, data = cpu_pretty, subset = NULL)
summary(cpu_mod.slm)

cpu <- as_tibble(cpu)

## Create a df of predictors
predictors <- cpu[,c("CycleTime", "MinimumMainMemory", "MaximumMainMemory",
       "CacheSize", "MinimumNumberOfChannels", "MaximumNumberOfChannels")]





# Linear

BestPred <- data.frame("Degree" = 1:6, "BestAttribute" = rep(NA, 6))

RSSVals <- data.frame(names(predictors), "RSS" = 1:length(predictors))
for (i in 1:length(predictors)) {
  
  RSSVals[i,2] <- (lm(cpu$Performance ~ poly(as_vector(predictors[i]), 1))$residuals)^2 %>% sum()
  
}
RSSVals



BestPred[1,2] <-  as.character(RSSVals[which.min(RSSVals[,2]),1][1])


# Lin to 6th deg
mdg <- 6


BestPred <- data.frame("Degree" = 1:(mdg+1), "BestAttribute" = rep(NA, (mdg+1)))

for (j in (1:mdg)) {
  
RSSVals <- data.frame(names(predictors), "RSS" = 1:length(predictors))
  for (i in 1:length(predictors)) {
    
    RSSVals[i,2] <- (lm(cpu$Performance ~ poly(as_vector(predictors[i]), j))$residuals)^2 %>% sum()
    
  }

BestPred[j,2] <-  as.character(RSSVals[which.min(RSSVals[,2]),1][1])
}

BestPred


# Hyperbolic

RSSVals <- data.frame(names(predictors), "RSS" = 1:length(predictors))
for (i in 1:length(predictors)) {
  
  RSSVals[i,2] <- (lm(cpu$Performance ~ (1/as_vector(predictors[i])))$residuals)^2 %>% sum()
  
}
RSSVals

RSSVals[which.min(RSSVals[,2]),1][1] %>% as.character()

BestPred$Degree[(mdg+1)] <- -1
BestPred[(mdg+1),2] <-  as.character(RSSVals[which.min(RSSVals[,2]),1][1])

BestPred





